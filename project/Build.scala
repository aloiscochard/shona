import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization        := "com.github.aloiscochard.shona",
    version             := "0.1-SNAPSHOT",
    scalaVersion        := "2.11.0-SNAPSHOT",
    scalaOrganization   := "org.scala-lang.macro-paradise",
    resolvers           ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    )
  )
}

object Dependencies {
  val testDependencies = Seq(
    libraryDependencies += "org.specs2" %% "specs2" % "1.13.1-SNAPSHOT" % "test" cross CrossVersion.full
  )
}


object ShonaBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val shona = Project (
    "shona",
    file ("."),
    settings = buildSettings ++ Seq(publishArtifact := false)
  ) aggregate (core, demo)

  lazy val core = Project(
    "shona-core",
    file("shona-core"),
    settings = buildSettings ++ testDependencies ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _)
    )
  )

  lazy val demo = Project(
    "shona-demo",
    file("shona-demo"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(core)
}
