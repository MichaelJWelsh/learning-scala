package environments

import zio.{Has, ZIO, ZLayer}

object addition {
  type Addition = Has[Addition.Service]

  object Addition {
  trait Service {
    def addTo(x: Int): ZIO[Any, Nothing, Int]
  }
    val live1: ZLayer[Any, Nothing, Addition] = ZLayer.succeed{
      new Service {
      override def addTo(x: Int): ZIO[Any, Nothing, Int] =
        ZIO.succeed(x + 5)
      }
    }

    val live2: ZLayer[Any, Nothing, Addition] = ZLayer.succeed{
      new Service {
        override def addTo(x: Int): ZIO[Any, Nothing, Int] =
          ZIO.succeed(x + 10)
      }
    }

    def addTo(x: Int): ZIO[Addition, Nothing, Int] =
      ZIO.accessM((addition: Addition) => addition.get.addTo(x))
  }
}
