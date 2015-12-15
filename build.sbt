name := "gentle-lms"

version := "1.0"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers ++= Seq(Resolver.sonatypeRepo("releases"), 
		  Resolver.sonatypeRepo("snapshots"))

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

libraryDependencies += "org.scala-lang.lms" %% "lms-core" % "0.9.0"

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

scalacOptions += "-Yvirtualize"

scalacOptions += "-deprecation"
