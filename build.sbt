//import com.github.retronym.SbtOneJar._

//oneJarSettings

name := "Hopper"

version := "0.1"

organization := "edu.colorado.plv"

scalaVersion := "2.10.2"

offline := true

//ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

scalacOptions ++= Seq("-deprecation", "-feature")

javacOptions += "-Xlint:unchecked"

resolvers += "Local Maven Repository" at "file:///"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "3.0.5",
	"edu.colorado.plv" %% "walautil" % "0.1-SNAPSHOT",
	"edu.colorado.plv" %% "droidel" % "0.1-SNAPSHOT",
	"com.ibm.wala" % "com.ibm.wala.shrike" % "1.5.2",
	"com.github.scopt" % "scopt_2.12" % "3.7.0",
	"com.ibm.wala" % "com.ibm.wala.util" % "1.5.2",
	"com.ibm.wala" % "com.ibm.wala.core" % "1.5.2",
  "com.ibm.wala" % "com.ibm.wala.dalvik" % "1.5.2",
	"com.twitter" %% "util-collection" % "6.12.1",
	"com.squareup" % "javawriter" % "2.2.1")
