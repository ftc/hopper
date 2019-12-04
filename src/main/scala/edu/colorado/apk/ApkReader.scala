package edu.colorado.apk

import java.io.{File, InputStream}
import java.net.{URI, URLEncoder}
import java.nio.file.{Files, Path, Paths}
import java.util

import com.ibm.wala.classLoader.{IClass, IClassLoader, IMethod, Language}
import com.ibm.wala.dalvik.classLoader.DexIRFactory
import com.ibm.wala.dalvik.ipa.callgraph.impl.AndroidEntryPoint
import com.ibm.wala.dalvik.util.AndroidAnalysisScope
import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.cha.{ClassHierarchy, ClassHierarchyFactory}
import com.ibm.wala.types.{ClassLoaderReference, MethodReference, TypeName, TypeReference}
import com.ibm.wala.dalvik.util.AndroidEntryPointLocator
import com.ibm.wala.dalvik.util.AndroidEntryPointLocator.LocatorFlags
import com.ibm.wala.ipa.callgraph.AnalysisOptions.ReflectionOptions
import com.ibm.wala.ipa.callgraph.impl.{DefaultContextSelector, Util}
import com.ibm.wala.ipa.callgraph.propagation.{InstanceKey, PointerAnalysis}
import com.ibm.wala.ipa.callgraph.propagation.cfa.ZeroXInstanceKeys
import com.ibm.wala.ssa.{DefaultIRFactory, IR}
import com.ibm.wala.util.debug.UnimplementedError
import edu.colorado.apk.ApkReader.ApkManifestClasses
import edu.colorado.walautil.cg.MemoryFriendlyZeroXContainerCFABuilder
import org.scandroid.util.{AndroidAnalysisContext, CLISCanDroidOptions, ISCanDroidOptions}

import scala.util.matching.Regex
import collection.JavaConversions
import sys.process._
import collection.JavaConverters._
import scala.annotation.tailrec
import scala.collection.immutable
import scala.xml.{Elem, NodeSeq, XML}

class IRWrapper(cha: ClassHierarchy, callGraph: Option[CallGraph],
                callbacks: List[AndroidEntryPoint], cache: AnalysisCache,
                manifest: ApkManifestClasses,
                pa : PointerAnalysis[InstanceKey]) {
  def getCallGraph = callGraph.getOrElse(???)
  def getPointerAnalysis = pa

  def getDirectCallbacks(clazz: IClass) ={
    clazz.getDeclaredMethods.asScala.filter(mostPreciseFmwkOverride(_).isDefined)
  }
  def isFramework(clazz: IClass): Boolean = {
    val name = clazz.getName.getPackage.toString
    !manifest.mainPackages.exists((p: String) => name.startsWith(p))
  }

  /**
   * For a given method, find the most precise overridden method in the framew¶ork.
   *
   * @param method method in the application
   * @return None if not an override, Some[IMethod] if exists
   */
  def mostPreciseFmwkOverride(method: IMethod): Option[IMethod] = {
    val declaringClass: IClass = method.getDeclaringClass
    if (isFramework(declaringClass))
      throw new IllegalArgumentException("Method is already in framework.")

    val name = method.getName.toString
    val retType: TypeReference = method.getReturnType
    val paramType: immutable.Seq[TypeReference] =
      (0 until method.getNumberOfParameters).map(index => method.getParameterType(index))

    iFindOverride(name, retType, paramType, declaringClass.getSuperclass)

    //TODO: interfaces
  }

  @tailrec
  private def iFindOverride(name: String, returnType: TypeReference,
                            paramTypes: Seq[TypeReference], declaringClass: IClass): Option[IMethod] = {
    if(declaringClass == null){
      None
    }else if (!isFramework(declaringClass)) {
      iFindOverride(name, returnType, paramTypes, declaringClass.getSuperclass)
    } else {
      // Overriding method must have same parameter types and name as framework class
      // Return type must be subtype of overridden method (compiler checks this)
      val mopt = declaringClass.getAllMethods.asScala.find(m => {
        (m.getName.toString == name) &&
          (0 until m.getNumberOfDefaultParameters)
            .forall(index => m.getParameterType(index) == paramTypes(index))
      } )
      mopt match {
        case Some(method) => Some(method)
        case None => iFindOverride(name, returnType, paramTypes, declaringClass.getSuperclass)
      }
    }
  }

  def javaSigToWalaSig(sig: String): Unit = {
    //    val ref = new TypeReference()
    val nplus: String = sig.split(" ")(1)
    val retType = ApkReader.typeToAbbr(sig.split(" ")(0))
    //sig is ()V for void onCreate()
    val sig_part = "(" + nplus.split('(')(1) + retType
    val name = nplus.split('(')(0)
    (name, sig_part)
  }

  def findMethod(sig: String, name: String, clazzName: String): IMethod = {

    val mref: MethodReference =
      MethodReference.findOrCreate(Language.JAVA, ClassLoaderReference.Application, clazzName, name, sig)
    //    val targets = cha.getPossibleTargets(mref)
    val m = cha.resolveMethod(mref)
    if (m == null) {
      throw new IllegalArgumentException(s"Method does not exist: ${clazzName + name + sig}")
    }
    m
  }
  def findClass(clazzName: String): IClass = {
    val typename = TypeName.findOrCreate(clazzName)
    val loader: IClassLoader = cha.getLoaders.filter(a => a.toString.contains("Application"))(0)
    val out = loader.lookupClass(typename)
    if (out != null) {
      out
    }else{
      throw new IllegalArgumentException(s"Class ${clazzName} does not exist")
    }
  }
  def findMethodFuzzy(sig:String = ".*", name:String = ".*", clazzName: String = ".*"): Set[IMethod] ={
    def otr(s: String) = new Regex(s)
    findMethodInSubClassFuzzy(cha.getRootClass,otr(sig),otr(name),otr(clazzName))
  }
  def findMethodInSubClassFuzzy(cl: IClass, sig : Regex, name : Regex, clazzName : Regex): Set[IMethod] = {
    val cur = findInClassMethodFuzzy(cl,sig,name,clazzName)
    cha.getImmediateSubclasses(cl).asScala.foldLeft(cur)( (acc,v) =>
      acc.union(findMethodInSubClassFuzzy(v,sig,name,clazzName)))
  }

  def findInClassMethodFuzzy(cl: IClass, sig : Regex, name : Regex, clazzName : Regex): Set[IMethod] = {
    clazzName.findFirstIn(cl.getName.toString).map(_ =>
    cl.getAllMethods().asScala.flatMap((a:IMethod) => {
      name.findFirstIn(a.getName.toString).flatMap(_=>
      {
        sig.findFirstIn(a.getSignature).map(_=>a)
      })
    }).toSet).getOrElse(Set())
  }

  def getManifestClasses():Set[IClass] = {
    manifest.activities.map(a => findClass(a))
  }

  // TODO: abstract IR?
  def getIR(m: IMethod) = {
    val methodref: MethodReference = m.getReference()
    val ir: IR = cache.getIR(m)
    if(ir == null){
      throw new IllegalStateException(s"IR for method ${m} not found")
    }else{ir}
  }
}

object ApkReader {
  val apkToolPath:String =
    Option(System.getenv("APK_TOOL_PATH"))
      .getOrElse(throw new RuntimeException("APK_TOOL_PATH environment variable is required"))
  val androidJar = new File(System.getenv("ANDROID_HOME") + "/platforms/android-23/android.jar")
  val exclusions = getClass.getResource("/wala_exclusions.txt").getPath

  case class ApkManifestClasses(mainPackages: Set[String], activities: Set[String])

  def decodeApkManifestClasses(androidApk: File) = {
    //Use CLI apk tool to avoid dependency conflict with wala

    val apkLoc:String = androidApk.getAbsolutePath.toString
    val tmpdirf: Path = Files.createTempDirectory("extract_apk")
    val outdir: String = tmpdirf.toAbsolutePath.toString
    val cmd : String = s"java -jar ${apkToolPath} d -o ${outdir} -f --no-src ${apkLoc}"
    if(( cmd ! ) != 0)
      throw new RuntimeException(s"Invocation of APKTool failed, (cmd: ${cmd}")
    val manifestFile: Elem = XML.load(outdir + "/" + "AndroidManifest.xml")
    val activities: immutable.Seq[String] = (manifestFile \\ "activity").flatMap(a => {
      a.attribute("http://schemas.android.com/apk/res/android", "name")
    }).map(a => "L" + a.toString.split("\\.").mkString("/"))

    val fragments = (manifestFile \\ "fragment")
    if (fragments.length > 0) {
      throw new UnimplementedError("XML Fragments")
    }
    val mf: NodeSeq = manifestFile \\ "manifest"
    val mainPackageSet = (mf \ "@package").map(_.toString).toSet
    (new File(outdir)).delete()
    ApkManifestClasses(mainPackageSet.map(a => a.toString.split("\\.").mkString("/")), activities.toSet)
  }

  def irFromAPK(androidApk: File, androidJarFile: File) = {

    // Get reflectively located classes from manifest
//    decodeApkManifestClasses(androidApk)

    // Load dalvik code
    val dalvikScope: AnalysisScope = makeDalvikScope(null, androidJarFile, androidApk.getAbsolutePath)
    val cha = ClassHierarchyFactory.make(dalvikScope)



    //Generate entrypoints
    val flags = Set[LocatorFlags](
      LocatorFlags.INCLUDE_CALLBACKS,
      LocatorFlags.CB_HEURISTIC,
      LocatorFlags.EP_HEURISTIC
    )
    val eps: util.List[AndroidEntryPoint] = new AndroidEntryPointLocator(flags.asJava).getEntryPoints(cha)
    val options = new AnalysisOptions(dalvikScope, eps)
    //TODO: reflection options relevant?
    options.setReflectionOptions(ReflectionOptions.STRING_ONLY)
    val cache: AnalysisCacheImpl = new AnalysisCacheImpl(new DexIRFactory())



    val cgb = Util.makeZeroCFABuilder(Language.JAVA, options, cache, cha, dalvikScope)
    val callGraph: CallGraph = cgb.makeCallGraph(options, null)

    val defaultInstancePolicy = ZeroXInstanceKeys.ALLOCATIONS | ZeroXInstanceKeys.SMUSH_MANY |
      ZeroXInstanceKeys.SMUSH_STRINGS | ZeroXInstanceKeys.SMUSH_THROWABLES
    val cfaBuilder = new MemoryFriendlyZeroXContainerCFABuilder(cha, options, cache, null,
      null, defaultInstancePolicy)
    cfaBuilder.makeCallGraph(options)
    val pa: PointerAnalysis[InstanceKey] = cfaBuilder.getPointerAnalysis

    new IRWrapper(cha, Some(callGraph), eps.asScala.toList, cache, decodeApkManifestClasses(androidApk), null)
  }

  def getScandroidPA(androidApk: File, androidJarFile: File):IRWrapper = {
    //TODO: delete this method, makezeroCFABuilder obviously broken
    val options = new CLISCanDroidOptions(Array("--android-lib", androidJarFile.getAbsolutePath, androidApk.getAbsolutePath),true)
    val cache: AnalysisCacheImpl = new AnalysisCacheImpl(new DexIRFactory())

    val ctx = new AndroidAnalysisContext(options,exclusions)
    val cha = ctx.getClassHierarchy
    val scope = ctx.getScope
//    val analysisOptions: ISCanDroidOptions = ctx.getOptions

    //Generate entrypoints
    val flags = Set[LocatorFlags](
      LocatorFlags.INCLUDE_CALLBACKS,
      LocatorFlags.CB_HEURISTIC,
      LocatorFlags.EP_HEURISTIC
    )

    val eps: util.List[AndroidEntryPoint] = new AndroidEntryPointLocator(flags.asJava).getEntryPoints(cha)
    val analysisOptions = new AnalysisOptions(scope, eps)

    val cgb2 = AndroidAnalysisContext.makeZeroCFABuilder(analysisOptions, cache, cha, scope,
      new DefaultContextSelector(analysisOptions, cha), null, List[InputStream]().asJava, null);

    ???
  }

  def readAPKCHA(apkloc: File) = {
    val clazz = ApkReader.getClass
    val clazzloader = clazz.getClassLoader()
    val analysisScope: AnalysisScope =
      AndroidAnalysisScope.setUpAndroidAnalysisScope(apkloc.toURI(), exclusions, clazzloader)


    val cha: ClassHierarchy = ClassHierarchyFactory.make(analysisScope)
    new IRWrapper(cha, None, List(), null, decodeApkManifestClasses(apkloc),null)
  }

  def typeToAbbr(ty: String): String = {
    val code = ty match {
      case v if v == "void" => TypeReference.VoidTypeCode.toChar
      case v if v == "boolean" => TypeReference.BooleanTypeCode.toChar
      case v if v == "byte" => TypeReference.ByteTypeCode.toChar
      case v if v == "short" => TypeReference.ShortTypeCode.toChar
      case v if v == "int" => TypeReference.IntTypeCode.toChar
      case v if v == "long" => TypeReference.LongTypeCode.toChar
      case v if v == "float" => TypeReference.FloatTypeCode.toChar
      case v if v == "double" => TypeReference.DoubleTypeCode.toChar
      case v if v == "char" => TypeReference.CharTypeCode.toChar
      case v => v
    }
    code.toString
  }

  //  def isApplication(tn: TypeName, pkg: Set[String]): Boolean =
  //    pkg.exists(a => {
  //      val packagename = tn.getPackage.toString
  //      packagename.startsWith(a) || packagename.tail.startsWith(a)
  //    }
  //    )
  //
  //  def isApplication(clazz: IClass, pkg: Set[String]): Boolean = isApplication(clazz.getName, pkg)


  case class ApkReadConfig(action: String = "", apkloc: File = null, pkg: Set[String] = Set(), outdir: File = null)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[ApkReadConfig]("ApkReader") {
      head("ApkReader", "0.1")
      opt[String]('m', "mode").required()
        .action((x, c) => c.copy(action = x))
      opt[File]('f', "file").required()
        .action((x, c) => c.copy(apkloc = x))
      opt[String]('p', "packages")
        .action((x, c) => c.copy(pkg = x.split(":").toSet))
      opt[File]('o', "outfile")
        .action((x,c) => c.copy(outdir=x))
    }
    parser.parse(args, ApkReadConfig()) match {
      case Some(ApkReadConfig("info", f, p,_)) => println(decodeApkManifestClasses(f))
      case Some(ApkReadConfig("datalog_out", f, _, outdir)) => {
        import java.io._
        val manifestClassFile = Paths.get(outdir.getAbsolutePath, "ManifestClass.facts");
        val pw = new PrintWriter(new File(manifestClassFile.toUri))
        val mf = decodeApkManifestClasses(f)
        mf.activities.foreach(a => pw.write(a.split("L")(1).split("\\/").mkString(".") + "\n"))
        pw.close()
        val packageRegexpFile = Paths.get(outdir.getAbsolutePath, "PackageRegexp.facts")
        val pw2 = new PrintWriter(new File(packageRegexpFile.toUri))
        mf.mainPackages.foreach(a => pw2.write(a.split("\\/").mkString(".") + ".*"))
        pw2.close()
      }
      case Some(ApkReadConfig(a, f, p, _)) if a == "cha" => readAPKCHA(f)
      case Some(ApkReadConfig(a, f, p, _)) if a == "findCallin" => {
        val pkgs = p.map((a: String) => a.map((a: Char) => if (a == '.') '/' else a))
        val cha = readAPKCHA(f)
      }
      case None =>
    }

  }

  import com.ibm.wala.classLoader.JarFileModule
  import com.ibm.wala.dalvik.util.AndroidAnalysisScope
  import com.ibm.wala.ipa.callgraph.AnalysisScope
  import com.ibm.wala.types.ClassLoaderReference
  import java.util.jar.JarFile

  import java.io.File
  import java.io.IOException

  //Used to load android jar
  @throws[IOException]
  def convertJarToDex(jarFile: String): File = {
    val f = File.createTempFile("convert", ".dex")
    System.err.println(f)
    com.android.dx.command.Main.main(Array[String]("--dex", "--output=" + f.getAbsolutePath, jarFile))
    f
  }

  @throws[IOException]
  def makeDalvikScope(androidLibs: File, androidAPIJar: File, dexFileName: String): AnalysisScope = {
    if (androidLibs != null)
      AndroidAnalysisScope.setUpAndroidAnalysisScope(new File(dexFileName).toURI,
        "", ApkReader.getClass.getClassLoader, androidLibs.toURI)
    else {
      val scope = AndroidAnalysisScope.setUpAndroidAnalysisScope(new File(dexFileName).toURI,
        "", ApkReader.getClass.getClassLoader)
      if (androidAPIJar != null) scope.addToScope(ClassLoaderReference.Primordial,
        new JarFileModule(new JarFile(androidAPIJar)))
      scope
    }
  }
}
