name := "scala-streams"

version := "1.0"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers ++= Seq(Resolver.sonatypeRepo("releases"), 
		  Resolver.sonatypeRepo("snapshots"))

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "org.scala-lang" % "scala-reflect" % "2.11.4",
  "EPFL" %% "lms" % "0.3-SNAPSHOT"
)

scalacOptions ++= Seq("-Yvirtualize", "-feature", "-deprecation")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "100", "-workers", "2", "-verbosity", "1")
