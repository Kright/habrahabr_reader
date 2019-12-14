name := "habrahabr_reader"
version := "0.1.0"
scalaVersion := "2.12.8"

// https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-explaintypes",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
//   "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
//   "-Ywarn-value-discard",  // Warn when non-Unit expression results are unused.
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "com.github.pureconfig" %% "pureconfig" % "0.11.1",

  "org.scala-lang.modules" %% "scala-xml" % "1.2.0",
  "com.bot4s" %% "telegram-akka" % "4.3.0-RC1",
  "net.ruippeixotog" %% "scala-scraper" % "2.1.0", // html parsing
  "org.typelevel" %% "cats-core" % "2.0.0-M1", // cats fp
  "ch.qos.logback" % "logback-classic" % "1.2.3", // logging backend

  "com.typesafe.akka" %% "akka-actor" % "2.5.22",
  "com.typesafe.akka" %% "akka-slf4j" % "2.5.22", // logging
  "com.typesafe.akka" %% "akka-testkit" % "2.5.22" % Test, // test actors
)
