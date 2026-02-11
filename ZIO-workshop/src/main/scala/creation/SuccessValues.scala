package creation

import zio.ZIO

object SuccessValues {

  //Examples from workshop
  val s1: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val s2: ZIO[Any, Nothing, Long] = ZIO.effectTotal(System.currentTimeMillis())

  //Working examples

  //create ZIOs of the following types
  val s3: ZIO[Any, Nothing, String] = ZIO.succeed("WOW")
  val s4: ZIO[Any, Nothing, List[String]] = ZIO.succeed(List("W", "O", "W"))
  val s5: ZIO[Any, Nothing, Option[Boolean]] = ZIO.succeed(Some(true))

}
