name := "jsonpicklers"

organization := "com.jteigen"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

description := "A pickler library for json"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.7.1" % "test"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

scalacOptions += "-unchecked"

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

licenses := Seq("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

homepage := Some(url("http://github.com/teigen/jsonpicklers"))

pomExtra := (
  <scm>
    <url>git@github.com:teigen/jsonpicklers.git</url>
    <connection>scm:git:git@github.com:teigen/jsonpicklers.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jteigen</id>
      <name>Jon-Anders Teigen</name>
      <url>http://jteigen.com</url>
    </developer>
  </developers>)
