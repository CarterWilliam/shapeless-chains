organization := "carter"

name := "func-chaining"

version := "0.0.1"

scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "com.chuusai"     %% "shapeless"    % "2.3.3",
  "org.scala-lang"  % "scala-library" % scalaVersion.value,
  "org.scala-lang"  % "scala-reflect" % scalaVersion.value,
  "org.typelevel"   %% "cats-core"    % "1.6.0",
  "org.specs2"      %% "specs2-core"  % "4.3.4"   % "test"
)
