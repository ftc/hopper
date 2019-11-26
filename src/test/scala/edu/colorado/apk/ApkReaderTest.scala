package edu.colorado.apk

import java.io.File
import java.net.URL

import com.ibm.wala.classLoader.IMethod
import org.scalatest.FunSuite

class ApkReaderTest extends FunSuite {
  private val url: URL = getClass.getResource("/trikita.slide_4.apk")
  val apkPath = url.getPath()

  val icha: IRWrapper = ApkReader.irFromAPK(new File(url.getPath),ApkReader.androidJar)
  val createMethod: IMethod = icha.findMethod("(Landroid/os/Bundle;)V", "onCreate",
    "Ltrikita/slide/ui/MainActivity")
  val activityCreate: IMethod = icha.findMethod("(Landroid/os/Bundle;)V", "onCreate", "Landroid/app/Activity")
  val nonCallbackClass = icha.findClass("Ltrikita/slide/Slide")
  val nonCallbackMethod:IMethod = icha.findMethod("(Ljava/lang/String;)Ljava/util/List;", "parse", "Ltrikita/slide/Slide;")
  test("Find Method in Framework CHA") {
    assert(activityCreate != null)
    val reference = activityCreate.getReference()
    val slideact: IMethod = icha.findMethod("(Landroid/os/Bundle;)V", "onCreate",
      "Ltrikita/slide/ui/MainActivity")
    assert(slideact != null)
    assert(!slideact.isSynthetic())
    assert(!slideact.isWalaSynthetic())
    icha.getIR(slideact)
  }
  test("Find method override in framework") {
    val over = icha.mostPreciseFmwkOverride(createMethod)
    assert(over.isDefined)
    assert(over.get.getDeclaringClass.getReference.toString.contains("Landroid/app/Activity"))
    val nonOver = icha.mostPreciseFmwkOverride(nonCallbackMethod)
    assert(nonOver.isEmpty)
  }
  test("Determine if a class is in the framework"){
    assert(!icha.isFramework(createMethod.getDeclaringClass))
    assert(icha.isFramework(activityCreate.getDeclaringClass))
    assert(!icha.isFramework(nonCallbackMethod.getDeclaringClass))
  }
  test("Throw error if method does not exist"){
    assertThrows[IllegalArgumentException](icha.findMethod("()V","foobar", "Landroid/app/Activity"))
  }
  test("Call Graph Contains"){
    val finishClickNpe: URL =
      getClass.getResource("/finish_npe.apk")
    val ir: IRWrapper = ApkReader.irFromAPK(new File(finishClickNpe.getPath), ApkReader.androidJar)



  }
  test("Find activities"){
    val manifestClasses = ApkReader.decodeApkManifestClasses(new File(apkPath))
    assert(manifestClasses.activities.contains("Ltrikita/slide/ui/MainActivity"))
    assert(manifestClasses.mainPackages.contains("trikita/slide"))
    def manifestClazzes = icha.getManifestClasses()
    assert(manifestClazzes.exists(a => a.toString.contains("MainActivity")))
  }
  test("Get Direct Callbacks should find activity callbacks"){
    val mainActivityclass = icha.findClass("Ltrikita/slide/ui/MainActivity")
    val dc = icha.getDirectCallbacks(mainActivityclass)
    List("onActivityResult","onBackPressed","<init>","onCreate").map({ b =>
      assert(dc.exists(a => a.toString.contains(b)))
    })
  }

}

