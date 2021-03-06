name := "AoC-2021"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "3.3.0",
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")