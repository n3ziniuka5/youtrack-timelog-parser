scalaVersion := "2.11.7"

val scalazVersion = "7.1.4"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz.stream" %% "scalaz-stream" % "0.7.2a",
  "joda-time" % "joda-time" % "2.8.2",
  "org.joda" % "joda-convert" % "1.8"
)
