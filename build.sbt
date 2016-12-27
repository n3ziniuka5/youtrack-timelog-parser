scalaVersion := "2.11.8"

val scalazVersion = "7.2.8"

version := "1.1.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz.stream" %% "scalaz-stream" % "0.8.5",
  "joda-time" % "joda-time" % "2.8.2",
  "org.joda" % "joda-convert" % "1.8",
  "org.specs2" %% "specs2-core" % "3.8.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")