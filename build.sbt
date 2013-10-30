name := "jsonpicklers"

organization := "com.jteigen"

scalaVersion := "2.10.3"

description := "A pickler library for json"

libraryDependencies += "net.liftweb" %% "lift-json" % "2.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

scalacOptions ++= Seq("-unchecked", "-feature", "-language:implicitConversions", "-language:higherKinds")

releaseSettings

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
