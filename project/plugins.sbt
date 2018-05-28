addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.4")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.2.2")

libraryDependencies ++= Seq(
  "org.jfree" % "jfreechart" % "1.0.14",
  "com.typesafe.play" %% "play-json" % "2.6.8"
)

