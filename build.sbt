scalaVersion := "2.12.1"

val scalazVersion = "7.2.9"

version := "1.2.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz.stream" %% "scalaz-stream" % "0.8.6",
  "org.specs2" %% "specs2-core" % "3.8.8" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")