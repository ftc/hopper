package edu.colorado.hopper.client

import java.io.{File, FileInputStream}
import java.util.jar.JarFile

import com.ibm.wala.analysis.pointers.HeapGraph
import com.ibm.wala.classLoader.{BinaryDirectoryTreeModule, IClass, IMethod}
import com.ibm.wala.dalvik.classLoader.DexIRFactory
import com.ibm.wala.ipa.callgraph.AnalysisOptions.ReflectionOptions
import com.ibm.wala.ipa.callgraph._
import com.ibm.wala.ipa.callgraph.impl.{ArgumentTypeEntrypoint, DefaultEntrypoint}
import com.ibm.wala.ipa.callgraph.propagation.cfa.ZeroXInstanceKeys
import com.ibm.wala.ipa.callgraph.propagation.{InstanceKey, PointerAnalysis}
import com.ibm.wala.ipa.cha.{ClassHierarchy, ClassHierarchyFactory, IClassHierarchy}
import com.ibm.wala.ssa.{InstanceOfPiPolicy, SSAOptions}
import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.util.config.FileOfClasses
import edu.colorado.hopper.client.Client._
import edu.colorado.hopper.executor.{DefaultSymbolicExecutor, SymbolicExecutor, TransferFunctions}
import edu.colorado.hopper.jumping.{DefaultJumpingSymbolicExecutor, JumpingTransferFunctions, RelevanceRelation}
import edu.colorado.hopper.synthesis.{SynthesisSymbolicExecutor, SynthesisTransferFunctions}
import edu.colorado.thresher.core._
import edu.colorado.walautil.cg.ImprovedZeroXContainerCFABuilder
import edu.colorado.walautil._

import scala.jdk.CollectionConverters._

object Client {
  protected val DEBUG = false
}

class WrappedWalaAnalysisResults(override val cg : CallGraph, pa : PointerAnalysis[InstanceKey])
  extends WalaAnalysisResults(cg, pa) {
  override val hg = new HeapGraphWrapper(pa, cg).asInstanceOf[HeapGraph[InstanceKey]]
}

abstract class Client[T](appPath : String, libPath : Option[String], mainClass : String, mainMethod : String,
                      isRegression : Boolean = false) {

  lazy protected val analysisScope = makeAnalysisScope()
  lazy protected val cha = ClassHierarchyFactory.make(analysisScope)

  def makeAnalysisCache : AnalysisCache = new AnalysisCacheImpl(new DexIRFactory())

  // do the actual work of the analysis
  def check : T

  def makeCallGraphAndPointsToAnalysis : WalaAnalysisResults = {
    if (DEBUG) println(s"Class hierarchy size is ${cha.getNumberOfClasses()}")
    val entrypoints = makeEntrypoints
    assert(!entrypoints.isEmpty,
           "No entrypoints found! Class " + mainClass + " with method " + mainMethod + " does not seem to exist")
    if (DEBUG) {
      println(s"Got ${entrypoints.size} entrypoints")
      entrypoints.foreach(e => println(e.getMethod()))
    }
    val options = makeOptions(analysisScope, entrypoints)
    val cache = makeAnalysisCache

    // finally, build the call graph and extract the points-to analysis
    val cgBuilder = makeCallGraphBuilder(options, cache, cha, analysisScope, isRegression)

    val ptTimer = new Timer
    ptTimer.start
    println("Building call graph")
    val cg = cgBuilder.makeCallGraph(options, null)
    ptTimer.stop
    println(s"Points-to analysis took ${ptTimer.time} seconds")
    ptTimer.clear
    if (DEBUG) println(CallGraphStats.getStats(cg))
    val pa = cgBuilder.getPointerAnalysis()
    SameReceiverEntrypoint.clearCachedArgs()
    new WrappedWalaAnalysisResults(cg, pa)
  }

  // add bypass logic that delegates to stubs if applicable
  def addBypassLogic(options : AnalysisOptions, analysisScope : AnalysisScope, cha : IClassHierarchy) : Unit = {
    val nativeSpec = JavaUtil.getResourceAsFile("natives.xml", getClass)
    assert(nativeSpec.exists(), "Can't find native spec")
    com.ibm.wala.ipa.callgraph.impl.Util.setNativeSpec(nativeSpec.getAbsolutePath)
    com.ibm.wala.ipa.callgraph.impl.Util.addDefaultBypassLogic(options, analysisScope,
      classOf[com.ibm.wala.ipa.callgraph.impl.Util].getClassLoader(), cha)
  }

  def makeCallGraphBuilder(options : AnalysisOptions, cache : AnalysisCache, cha : IClassHierarchy,
                           analysisScope : AnalysisScope, isRegression : Boolean) : CallGraphBuilder[InstanceKey] = {
    assert(options.getMethodTargetSelector() == null, "Method target selector should not be set at this point.")
    assert(options.getClassTargetSelector() == null, "Class target selector should not be set at this point.")
    com.ibm.wala.ipa.callgraph.impl.Util.addDefaultSelectors(options, cha)
    addBypassLogic(options, analysisScope, cha)

    val defaultInstancePolicy = ZeroXInstanceKeys.ALLOCATIONS | ZeroXInstanceKeys.SMUSH_MANY |
      ZeroXInstanceKeys.SMUSH_STRINGS | ZeroXInstanceKeys.SMUSH_THROWABLES
    val instancePolicy =
      if (Options.PRIM_ARRAY_SENSITIVITY) defaultInstancePolicy
      else (defaultInstancePolicy | ZeroXInstanceKeys.SMUSH_PRIMITIVE_HOLDERS)
    new ImprovedZeroXContainerCFABuilder(cha, options, cache, null, null, instancePolicy)
  }

  def makeOptions(analysisScope : AnalysisScope, entrypoints : Iterable[Entrypoint]) : AnalysisOptions = {
    val javaEntryPoints = entrypoints.toList.asJava
    val collectionEntrypoints : java.util.Collection[_ <: Entrypoint] = javaEntryPoints
    val options = new AnalysisOptions(analysisScope, collectionEntrypoints)
    // turn off handling of Method.invoke(), which dramatically speeds up pts-to analysis
    options.setReflectionOptions(ReflectionOptions.NO_METHOD_INVOKE)
    if (Options.USE_PI_NODES) {
      //  use WALA's pi nodes to get cheap and easy handling of instanceof guards for cast checking 
      val ssaOpt = SSAOptions.defaultOptions()
      ssaOpt.setPiNodePolicy(InstanceOfPiPolicy.createInstanceOfPiPolicy())
      options.setSSAOptions(ssaOpt)
    }
    options
  }

  def isEntrypointClass(c : IClass, analysisScope : AnalysisScope, mainClass : String) : Boolean =
    // do contains(mainClass) instead of equality to account for WALA adding 'L' to front of each class name
    !ClassUtil.isLibrary(c) && c.getName().toString().contains(mainClass)

  def isEntrypoint(m : IMethod, cha : IClassHierarchy, mainMethod : String) : Boolean =
    m.getName().toString() == mainMethod

  // this creates concrete allocations for non-interface types and passes null (!) for interface types
  def mkDefaultEntrypoint(m : IMethod, cha : IClassHierarchy) : Entrypoint =
    new DefaultEntrypoint(m, cha)

  // this always create *a* concrete allocation of and interface (if possible) to pass as arguments to the entrypoint,
  // which is nicer for the pt analysis
  def mkArgumentTypeEntrypoint(m : IMethod, cha : IClassHierarchy) : Entrypoint =
    new ArgumentTypeEntrypoint(m, cha)

  // this goes one step beyond ArgumentTypeEntrypoint and passes *all* concrete allocatiAons of an interface in scope
  // as arguments. in addition, it considers aliasing among arguments passed in externally in order to maximize the
  // amount of behavior we see this is *especially* important for Android since entrypoints are event handler methods on
  // OS-level objects (like Activity's). if we don't consider the possibility that each event handler is called on the
  // same Activity, we will (unsoundly) miss a lot of behavior
  def mkSharedAllocationEntrypoint(m : IMethod, cha : IClassHierarchy) : Entrypoint =
    new SharedAllocationEntrypoint(m, cha)

  def mkEntrypoint(m : IMethod, cha : IClassHierarchy) : Entrypoint = {
    if (DEBUG) println(s"Making entrypoint $m")
    // IMPORTANT! for maximally evil synthesis, we want DefaultEntrypoint rather than SharedAllocationEntrypoint. this
    // is because we don't want the analysis to assume one of the concrete types in the class hierarchy was the
    // interface implementation that was used; we want the leeway to create our own implementation. for less adversarial
    // synthesis, we may want to make the opposite choice
    if (Options.SYNTHESIS) mkDefaultEntrypoint(m, cha) else mkSharedAllocationEntrypoint(m, cha)
  }

  def makeEntrypoints : Iterable[Entrypoint] = {
    def addMethodsToEntrypoints(methods : Iterable[IMethod], entrypoints : List[Entrypoint]) : List[Entrypoint] =
      methods.foldLeft (entrypoints) ((entrypoints, m) =>
        if (isEntrypoint(m, cha, mainMethod)) mkEntrypoint(m, cha) :: entrypoints else entrypoints)

    cha.asScala.foldLeft (List.empty[Entrypoint]) ((entrypoints, c) =>
      if (isEntrypointClass(c, analysisScope, mainClass)) addMethodsToEntrypoints(c.getDeclaredMethods().asScala, entrypoints)
      else entrypoints
    )
  }

  def makeAnalysisScope(addJavaLibs : Boolean = true) : AnalysisScope = {
    val analysisScope = AnalysisScope.createJavaAnalysisScope()
    def isJar(f : File) : Boolean = f.getName().endsWith(".jar")
    def isClassfile(f : File) : Boolean = f.getName().endsWith(".class")

    def addToScope(path : String, loader : ClassLoaderReference) : Unit = {
      val f  = new File(path)
      assert(f.exists(), s"Can't find file $f; it doesn't appear to exist")
      if (f.isDirectory()) {
        // add all .class files to the scope
        analysisScope.addToScope(loader, new BinaryDirectoryTreeModule(f))
        // add all jar files in top-level directory or sub-directory to the scope
        Util.getAllFiles(f, isJar).foreach(jar => {
          println(s"adding jar $jar")
          analysisScope.addToScope(loader, new JarFile(jar))
        })
      } else if (isJar(f)) analysisScope.addToScope(loader, new JarFile(f))
      else if (isClassfile(f)) analysisScope.addClassFileToScope(loader, f)
      else Util.unimp(s"Processing file $f. Expecting path to Java bytecode directory or JAR archive")
    }

    // add application code to analysis scope
    addToScope(appPath, analysisScope.getApplicationLoader())
    // add library code to analysis scope, if any
    libPath match {
      case Some(libPath) =>
        if (DEBUG) println(s"Adding lib file $libPath")
        addToScope(libPath, analysisScope.getPrimordialLoader())
      case None =>
        if (DEBUG) println("No library code specified.")
        () // no library code specified
    }

    if (addJavaLibs) getJVMLibFile match {
      // add core Java libraries
      case Some(libFile) =>
        if (DEBUG) println(s"Using JVM lib file $libFile")
        analysisScope.addToScope(analysisScope.getPrimordialLoader(), new JarFile(libFile))
      case None => sys.error("Can't find path to Java libraries. Exiting.")
    }

    // add WALA stubs
    getWALAStubs match {
      case Some(stubFile) => analysisScope.addToScope(analysisScope.getPrimordialLoader, new JarFile(stubFile))
      case None => sys.error("Can't find WALA stubs. Exiting.")
    }

    setExclusions(analysisScope)
    analysisScope
  }

  def setExclusions(analysisScope : AnalysisScope) : Unit = {
    // set exclusions if appropriate
    val exclusionsFile = new File(Options.EXCLUSIONS)
    if (exclusionsFile.exists()) {
      if (DEBUG) println(s"Using specified exclusions file ${exclusionsFile.getAbsolutePath()}")
      analysisScope.setExclusions(new FileOfClasses(new FileInputStream(exclusionsFile)))
    } else if (Options.EXCLUSIONS.equals(Options.DEFAULT_EXCLUSIONS)) {
    // look up default exclusions in the resources
      if (DEBUG) println("Using default exclusions file")
      val exclStream = getClass.getResourceAsStream(s"${File.separator}${Options.EXCLUSIONS}")
      analysisScope.setExclusions(new FileOfClasses(exclStream))
    } else if (DEBUG)
        println(s"Exclusions file ${exclusionsFile.getAbsolutePath()} does not exist, not using exclusions")
  }

  def makeTransferFunctions(walaRes : WalaAnalysisResults) : TransferFunctions =
    new TransferFunctions(walaRes.cg, walaRes.hg, walaRes.hm, walaRes.cha)

  def makeSymbolicExecutor(walaRes : WalaAnalysisResults) : SymbolicExecutor =
    if (Options.JUMPING_EXECUTION) {
      val rr = new RelevanceRelation(walaRes.cg, walaRes.hg, walaRes.hm, walaRes.cha)
      val tf = new JumpingTransferFunctions(walaRes.cg, walaRes.hg, walaRes.hm, walaRes.cha, rr)
      new DefaultJumpingSymbolicExecutor(tf, rr)
    } else if (Options.SYNTHESIS)
      new SynthesisSymbolicExecutor(new SynthesisTransferFunctions(walaRes.cg, walaRes.hg, walaRes.hm, walaRes.cha))
    else new DefaultSymbolicExecutor(makeTransferFunctions(walaRes))

   def getJVMLibFile : Option[File] = new File(Options.JAVA_LIB) match {
    case f if f.exists() => Some(f)
    case _ =>
      val PATH = System.getProperty("java.home")
      List(new File(PATH + "/lib/rt.jar"), new File(PATH + "/../Classes/classes.jar")).find(f => f.exists())
  }

  def getWALAStubs : Option[File] = {
    val f = JavaUtil.getResourceAsFile("primordial.jar.model", getClass)
    if (f.exists()) Some(f) else None
  }

}

abstract class ClientTests {
  def runRegressionTests() : Unit = ()
  // is this client compatible with piecewise execution? this should be true for most refutation-oriented clients and
  // false for most witness-oriented ones
  def isPiecewiseCompatible : Boolean = true

  protected def printTestFailureMsg(test : String, testNum : Int) : Unit = println(s"Test $test (#$testNum) failed :(")

  protected def getJVMVersion : String = System.getProperty("java.version")

}
