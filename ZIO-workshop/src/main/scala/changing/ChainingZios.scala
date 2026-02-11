package changing

import zio.ZIO

object ChainingZios {

  // Examples from workshop
  val zInt1: ZIO[Any, Nothing, Int] = ZIO.succeed(5)
  val g: Int => ZIO[Any, Nothing, Int] = int => ZIO.succeed(int+1)
  val zInt2: ZIO[Any, Nothing, Int] = zInt1.flatMap(g)

  val zInt: ZIO[Any, Nothing, Int] =
    for {
      startInt <- ZIO.succeed(5)
      nextInt <- ZIO.succeed(startInt + 1)
    } yield nextInt


  // Working examples
  val intList: ZIO[Any, Nothing, List[Int]] = ZIO.succeed(List(1,2,3,4,5))

  val doubleInt: Int => Int = x => 2*x

  val intsToStrings: List[Int] => ZIO[Any, Nothing, List[String]] =
    ints => ZIO.succeed(ints.map(_.toString))

  val printIntegerStrings: List[String] => ZIO[Any, Nothing, Unit] =
    strings => ZIO.succeed(strings.foreach(println(_)))


  //chain the functions above together to first double all the ints in the list and then move them to strings
  val doubledIntsAsStrings: ZIO[Any, Nothing, List[String]] =
    for {
      ints <- intList
      doubleInts = ints.map(doubleInt)
      doubleIntsAsStrings <- intsToStrings(doubleInts)
    } yield doubleIntsAsStrings

}
