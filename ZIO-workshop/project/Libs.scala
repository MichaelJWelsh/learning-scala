import sbt._

object Libs {

  val sttpVersion = "3.3.13"
  val zioVersion = "1.0.10"

  val libraryDependencies = Seq(
    "net.codingwell" %% "scala-guice" % "4.2.6",
    "com.beachape" %% "enumeratum" % "1.7.0",
    "com.beachape" %% "enumeratum-play-json" % "1.7.0",
    "com.softwaremill.sttp.client3" %% "core" % sttpVersion,
    "com.softwaremill.sttp.client3" %% "play-json" % sttpVersion,
    "com.softwaremill.sttp.client3" %% "okhttp-backend" % sttpVersion,
    "org.slf4j" % "slf4j-api" % "1.7.32",
    "ai.x" %% "play-json-extensions" % "0.42.0",
    "org.typelevel" %% "cats-core" % "2.6.1",
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-test" % zioVersion,
    "dev.zio" %% "zio-test-sbt" % zioVersion,
    "dev.zio" %% "zio-streams" % zioVersion,
    "dev.zio" %% "zio-interop-cats" % "2.5.1.0",
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
    "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    "org.scalatest" %% "scalatest-wordspec" % "3.2.9" % Test,
    "org.scalatestplus.play" %% "scalatestplus-play" % "5.0.0" % Test,
  )
}