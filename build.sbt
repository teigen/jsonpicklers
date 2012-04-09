scalaVersion := "2.9.1"

name := "jsonpicklers"

organization := "com.jteigen"

version := "0.1"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

scalacOptions += "-unchecked"
