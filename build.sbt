name := "scalax-parstream"

version := "0.1-SNAPSHOT"

organization := "org.scala-tools"

scalaVersion := "2.9.1"

scalacOptions += "-unchecked"

scalacOptions += "-deprecation"

libraryDependencies += "org.specs2" %% "specs2" % "1.6.1" % "test"

libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.5" % "test"

libraryDependencies += "net.databinder" %% "dispatch-lift-json" % "0.8.5" % "test"

resolvers += "Scala-Tools Maven2 Releases Repository" at "http://scala-tools.org/repo-releases"

resolvers += "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
