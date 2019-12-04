package edu.colorado.apk

import java.io.File

import scala.collection.mutable
import scala.sys.process._

object TestUtils {
  var buildCache = mutable.Map[String,File]()
  def buildApk(resName : String): File ={
    buildCache.getOrElse(resName,
      {
        val dir = getClass.getResource(s"/${resName}").getPath
        val runbuild = getClass.getResource("/runbuild.sh").getPath

        if ((s"bash ${runbuild} ${dir}" !) != 0)
          throw new RuntimeException(s"Building test project failed: ${resName}")
        val apkfile = new File(s"${dir}/bin/app.apk")
        assert(apkfile.exists())
        buildCache.put(resName, apkfile)
        apkfile
      }
    )
  }
}
