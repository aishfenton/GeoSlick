import sbt._
import sbt.Keys._

import sbtrelease.ReleasePlugin._

object GeoslickBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    organization := "com.azavea.geotrellis",
    version := "0.1.1-SNAPSHOT",
    scalaVersion := "2.10.0"
  )

  lazy val geoslick = Project(
    id = "geoslick",
    base = file("."),
    settings = Project.defaultSettings ++ releaseSettings ++ Seq(
      resolvers ++= Seq(
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
        Resolver.sonatypeRepo("snapshots")),
      libraryDependencies ++= Seq(
        "org.slf4j" % "slf4j-api" % "1.6.4",
        "com.typesafe.slick" %% "slick" % "1.0.0",
        "postgresql" % "postgresql" % "9.1-901.jdbc4",
        "com.vividsolutions" % "jts" % "1.13",
        "org.scalatest" % "scalatest_2.9.0" % "2.0.M5" % "test"),
      scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
      javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation"),
      parallelExecution in Test := false,
      publishMavenStyle := true,
      publishTo <<= (version){ v =>
        val nexus = "https://oss.sonatype.org/"
        if (v.trim.endsWith("SNAPSHOT"))
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },
      publishArtifact in Test := false,
      pomIncludeRepository := { _ => false },
      pomExtra :=
        <developers>
          <developer>
            <id>ahinz</id>
            <name>Adam Hinz</name>
            <url>http://github.com/ahinz/</url>
          </developer>
        </developers>
        <scm>
          <url>git@github.com:ahinz/GeoSlick.git</url>
          <connection>scm:git:git@github.com:ahinz/GeoSlick.git</connection>
        </scm>,
      licenses += "BSD" -> url("https://github.com/ahinz/GeoSlick/blob/master/LICENSE")
    )
  )
}
