lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.3"
    )),
    name := "scala-tree"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test
