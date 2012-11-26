name := "jsonpicklers"

organization := "com.jteigen"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2")

description := "A pickler library for json"

libraryDependencies += "org.json4s" %% "json4s-ast" % "3.1.0-SNAPSHOT"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.1.0-SNAPSHOT" % "test"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

scalacOptions += "-unchecked"

resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

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
