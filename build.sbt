name := "habrahabr_reader"
version := "0.1.0"
scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "com.github.pureconfig" %% "pureconfig" % "0.11.1",
  "com.typesafe.akka" %% "akka-actor" % "2.5.22",
  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
	"com.bot4s" %% "telegram-akka" % "4.3.0-RC1",
	"com.softwaremill.sttp" %% "core" % "1.6.4",
  "net.ruippeixotog" %% "scala-scraper" % "2.1.0", // html parsing
  "org.typelevel" %% "cats-core" % "2.0.0-M1", // cats fp

  "com.typesafe.akka" %% "akka-slf4j" % "2.5.22", // logging
  "ch.qos.logback" % "logback-classic" % "1.2.3", // logging backend
)
