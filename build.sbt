name := "shona"

version := "0.1"

scalaVersion := "2.10.0-RC1"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.14" % "test",
  "org.json4s" %% "json4s-native" % "3.1.0",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "com.h2database" % "h2" % "1.3.166"
)

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

