package edu.colorado.hopper.client
import java.io.File

import edu.colorado.apk.TestUtils
import edu.colorado.hopper.client.android.AndroidNullDereferenceClient
import edu.colorado.hopper.client.android.AndroidNullDereferenceClientTests.printTestFailureMsg
import edu.colorado.thresher.core.Options
import edu.colorado.walautil.{LoopUtil, Timer}
import org.scalatest.FunSuite

import sys.process._
import scala.sys.process.Process
class NullDereferenceClientTest extends FunSuite {


  val apk0 = TestUtils.buildApk("test_0")
  test("Build and Load Test Project (unit test test)"){
    assert(apk0.exists())
  }
  test("AndroidNullDereferenceClient") {
    //    val regressionDir = new File("src/test/java/nulls/")
//    val ndc = new NullDereferenceClient(apk0, new File("lib/droidel_android-4.4.2_r1.jar"), true)
  }

}
