val scala3 = "3.7.1"

ThisBuild / scalaVersion     := scala3
ThisBuild / organization     := "com.dixa"
ThisBuild / organizationName := "dixa"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6.2" withSources () withJavadoc ()
