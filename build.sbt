name := "sc"

version := "1.0-SNAPSHOT"

scalaVersion := "2.9.2"

organization := "com.mosesn"

libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.7.1" % "test",
                    "junit" % "junit" % "4.8.1" % "test",
                    "com.mosesn" %% "pirate" % "0.1.0")

scalacOptions += "-deprecation"


