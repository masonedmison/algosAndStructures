ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0",
      "org.typelevel" %% "cats-collections-core" % "0.9.0",
      "com.disneystreaming" %% "weaver-cats" % "0.7.11" % Test
    )
  )
