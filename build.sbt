ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "n2t-compiler",
    idePackagePrefix := Some("com.github.nishi_7")
  )

resolvers += Resolver.githubPackages("pois0", "pg4scala")
githubTokenSource := TokenSource.Environment("GITHUB_TOKEN") || TokenSource.GitConfig("github.token")

libraryDependencies += "jp.pois" %% "pg4scala-core" % "0.3.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.11" % "runtime"
