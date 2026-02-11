package creation

import zio.ZIO

object FailureValues {

  //Examples from workshop
  val f1: ZIO[Any, String, Nothing] = ZIO.fail("Boom!")
  val f2: ZIO[Any, Throwable, Nothing] = ZIO.fail(new Exception("Boom!"))

  //Working examples

  sealed trait Error

  case object Error1 extends Error
  case object Error2 extends Error

  //create ZIOs of the following types
  val f3: ZIO[Any, Int, Nothing] = ZIO.fail(-1)
  val f4: ZIO[Any, Error, Nothing] = ZIO.fail(Error1)

}
