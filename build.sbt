import com.typesafe.sbt.pgp.PgpKeys.publishSigned

// Convenient setting that allows writing `set scalaVersion := dotty.value` in sbt shell to switch from Scala to Dotty
val dotty = settingKey[String]("dotty version")
dotty in ThisBuild := "0.7.0-RC1"

val collectionsScalaVersionSettings = Seq(
  scalaVersion := "2.13.0-M4",
  crossScalaVersions := scalaVersion.value :: dotty.value :: Nil
)

val commonSettings = Seq(
  organization := "ch.epfl.scala",
  name := "collection-benchmark",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.13.0-M4",
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
  publish := (()),
  publishLocal := (()),
  pomExtra :=
    <developers>
      <developer><id>ichoran</id><name>Rex Kerr</name></developer>
      <developer><id>odersky</id><name>Martin Odersky</name></developer>
      <developer><id>pathikrit</id><name>Pathikrit Bhowmick</name></developer>
      <developer><id>julienrf</id><name>Julien Richard-Foy</name></developer>
      <developer><id>szeiger</id><name>Stefan Zeiger</name></developer>
      <developer><id>msteindorfer</id><name>Michael J. Steindorfer</name></developer>
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
  .settings(commonSettings)
     // Dotty 0.3.0-RC1 crashes when trying to compile this project
    //.settings(disableDotty)
    .settings(
      charts := Def.inputTaskDyn {
        val benchmarks = Def.spaceDelimited().parsed
        val targetDir = crossTarget.value
        val jmhReport = targetDir / "jmh-result.json"
        val runTask = run in Jmh
        Def.inputTask {
          val _ = runTask.evaluated
          scala.collection.benchmark.Bencharts(jmhReport, "Execution time (lower is better)", targetDir)
          targetDir
        }.toTask(s" -rf json -rff ${jmhReport.absolutePath} ${benchmarks.mkString(" ")}")
      }.evaluated
    )

val memory = project.in(file("memory"))
  .dependsOn(collections)
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(
    //libraryDependencies += ("org.spire-math" %% "jawn-ast" % "0.11.1-SNAPSHOT").withDottyCompat(scalaVersion.value),
    charts := Def.inputTaskDyn {
      val targetDir = crossTarget.value
      val report = targetDir / "report.json"
      val runTask = run in Compile
      Def.inputTask {
        val _ = runTask.evaluated
        scala.collection.benchmark.Bencharts(report, "Memory footprint (lower is better)", targetDir)
        targetDir
      }.toTask(s" ${report.absolutePath}")
  }.evaluated)

lazy val charts = inputKey[File]("Runs the benchmarks and produce charts")
