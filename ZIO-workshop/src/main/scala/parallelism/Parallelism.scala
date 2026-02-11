package parallelism

import zio.clock.Clock
import zio.{UIO, ZIO}

import java.time.Duration

object Parallelism {

  val fortyTwo: UIO[Int] = ZIO.succeed(42)
  val thirtySeven: UIO[Int] = ZIO.succeed(37)

  val numbersAsStrings: List[String] = List("42", "37", "73", "twelve", "21", "1", "10", "100")

  val numbersAsInts1: List[Int] = numbersAsStrings.flatMap(_.toIntOption)

  def toIntAndWait(str: String): ZIO[Clock, Option[Nothing], Int] =
    for {
      intOpt <- ZIO.fromOption(str.toIntOption)
      _ <- ZIO.sleep(Duration.ofSeconds(5))
    } yield intOpt

  val numbersAsInts: ZIO[Clock, Nothing, List[Int]] = ZIO.collect(numbersAsStrings)(toIntAndWait)
  val numbersAsIntsButQuick: ZIO[Clock, Nothing, List[Int]] = ZIO.collectPar(numbersAsStrings)(toIntAndWait)

  def printAndWait(str: String): ZIO[Clock, Nothing, Unit] =
    for {
      _ <- ZIO.succeed(println(str))
      _ <- ZIO.sleep(Duration.ofSeconds(1))
    } yield ()

  val printNumbers: ZIO[Clock, Nothing, Unit] = ZIO.foreach_(numbersAsStrings)(printAndWait)
  val printNumbersButQuick: ZIO[Clock, Nothing, Unit] = ZIO.foreachPar_(numbersAsStrings)(printAndWait)


  val number = ZIO.succeed(5)
  val numbers = List(42, 37, 73, 21, 1, 10, 11).map(ZIO.succeed(_))
  def largestNumber(a: Int, b: Int): Int = zio.Runtime.default.unsafeRun{
    ZIO.sleep(Duration.ofSeconds(1)).as(a max b)
  }

  val maxNumber = ZIO.reduceAll(number, numbers)(largestNumber)
  val maxNumberButQuick = ZIO.reduceAllPar(number, numbers)(largestNumber)

}
