name := "jsonpicklers"

organization := "com.jteigen"

scalaVersion := "2.10.0"

crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.10.0")

description := "A pickler/parser library for json4s"

libraryDependencies += "org.json4s" %% "json4s-ast" % "3.1.0" cross CrossVersion.binary

libraryDependencies ++= Seq(
    "org.json4s" %% "json4s-native" % "3.1.0" % "test" cross CrossVersion.binary,
    "org.scalatest" %% "scalatest" % "1.8" % "test" cross CrossVersion.full,
    "org.scalacheck" %% "scalacheck" % "1.10.0" % "test" cross CrossVersion.full)

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions <++= (scalaBinaryVersion).map{
    case "2.10" => Seq("-language:implicitConversions", "-language:higherKinds")
    case _      => Nil
}

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
