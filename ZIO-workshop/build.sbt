name := "zioWorkshops"
organization := "robin"

version := "0.1"

scalaVersion := "2.13.7"
scalacOptions := Settings.compilerOptions

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
libraryDependencies ++= Libs.libraryDependencies