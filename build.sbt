import com.typesafe.sbt.SbtStartScript

name := "hosc"

scalaVersion := "2.8.0"

mainClass in Compile := Some("hosc.EqProverApp")

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3" % "test"

seq(SbtStartScript.startScriptForClassesSettings: _*)
