//import com.github.retronym.SbtOneJar._

//oneJarSettings

name := "Hopper"

version := "0.1"

organization := "edu.colorado"

//scalaVersion := "2.10.2"
scalaVersion := "2.13.1"

//offline := true

//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

//scalacOptions ++= Seq("-deprecation", "-feature")

//javacOptions += "-Xlint:unchecked"

//resolvers += "Local Maven Repository" at "file:///"+Path.userHome.absolutePath+"/.m2/repository"

//"org.scalatest" %% "scalatest" % "3.0.5",

libraryDependencies ++= Seq(
//	"com.twitter" %% "util-collection" % "6.12.1",
	"org.apache.commons"%"commons-collections4"%"4.4",
	"edu.colorado.plv" %% "walautil" % "0.1-SNAPSHOT",
	"edu.colorado.plv" %% "droidel" % "0.1-SNAPSHOT",
	"com.ibm.wala" % "com.ibm.wala.shrike" % "1.5.2",
	"com.github.scopt" % "scopt_2.12" % "3.7.0",
	"com.ibm.wala" % "com.ibm.wala.util" % "1.5.2",
	"com.ibm.wala" % "com.ibm.wala.core" % "1.5.2",
	"com.ibm.wala" % "com.ibm.wala.dalvik" % "1.5.2",
	"com.squareup" % "javawriter" % "2.2.1",
	"com.jakewharton.android.repackaged" % "dalvik-dx" % "9.0.0_r3",
	"org.slf4j" % "slf4j-simple" % "1.7.29",
	"com.ibm.wala" % "com.ibm.wala.scandroid" % "1.5.2")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
