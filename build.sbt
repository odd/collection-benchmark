// Convenient setting that allows writing `set scalaVersion := dotty.value` in sbt shell to switch from Scala to Dotty
val dotty = settingKey[String]("dotty version")
dotty in ThisBuild := "0.19.0-RC1"

resolvers in ThisBuild ++= Seq(
  "scala-pr-validation-snapshots" at "https://scala-ci.typesafe.com/artifactory/scala-pr-validation-snapshots/",
  "scala-integration" at "https://scala-ci.typesafe.com/artifactory/scala-integration/")

val collectionsScalaVersionSettings = Seq(
  crossScalaVersions := scalaVersion.value :: dotty.value :: Nil
)

val commonSettings = Seq(
  organization := "org.scala-lang",
  name := "collection-benchmark",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds"/*, "-opt:l:classpath"*/),
  scalacOptions ++= {
    if (!isDotty.value)
      Seq("-opt-warnings") // This option does not exist in Dotty
    else
      Seq()
  },
  scalacOptions in (Compile, doc) ++= Seq("-implicits", "-groups"),
  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
  fork in Test := true,
  parallelExecution in Test := false,
  homepage := Some(url("https://github.com/odd/collection-benchmark")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/odd/collection-benchmark"),
      "scm:git:git@github.com:odd/collection-benchmark.git"
    )),
  publishArtifact := false,
  // The above is enough for Maven repos but it doesn't prevent publishing of ivy.xml files
  publish := ((): Unit),
  publishLocal := ((): Unit),
  pomExtra :=
    <developers>
      <developer><id>odd</id><name>Odd Möller</name></developer>
    </developers>
)

val collections = project.in(file("collections"))
  .settings(commonSettings)
  .settings(
    collectionsScalaVersionSettings,
    name := "collections",
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % Test
    )
)

val time = project.in(file("time"))
  .dependsOn(collections)
  .enablePlugins(JmhPlugin)
  .settings(mainClass in (Jmh, run) := Some("collection.benchmark.JmhRunner"))
  .settings(commonSettings)

val memory = project.in(file("memory"))
  .dependsOn(collections)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
