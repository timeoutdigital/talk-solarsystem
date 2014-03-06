import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization        := "com.timeout",
    version             := "0.1-SNAPSHOT",
    scalaVersion        := "2.10.3",
    resolvers           ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    )
  )
}

object Dependencies {
  val testDependencies = Seq(
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0"
  )
}


object TalkBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  lazy val talk = Project (
    "talk-solarsystem",
    file ("."),
    settings = buildSettings ++ Seq(publishArtifact := false)
  ) aggregate (common, solrs, solrsDemo)

  lazy val common = Project(
    "common",
    file("common"),
    settings = buildSettings ++ testDependencies ++ Seq(
      libraryDependencies ++= Seq(
        "net.liftweb" %% "lift-json" % "2.6-M2"
      )
    )
  )

  lazy val solrs = Project(
    "solrs",
    file("solrs"),
    settings = buildSettings ++ testDependencies ++ Seq(
      libraryDependencies ++= Seq(
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.2",
        "com.spatial4j" % "spatial4j" % "0.3",
        "org.apache.httpcomponents" % "httpclient" % "4.3.1",
        "org.apache.solr" % "solr-solrj" % "4.5.0"
      )
    )
  ) dependsOn(common)

  lazy val solrsDemo = Project(
    "solrs-demo",
    file("solrs-demo"),
    settings = buildSettings ++ testDependencies
  ) dependsOn(common, solrs)

}
