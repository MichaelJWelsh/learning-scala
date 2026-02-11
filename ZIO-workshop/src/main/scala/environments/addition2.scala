package environments

import zio.console.Console
import zio.{Has, ZIO, ZLayer}

object addition2 {
  type Addition = Has[Addition.Service]

  object Addition {
    trait Service {
      def addAndPrint(x: Int): ZIO[Any, Nothing, Unit]
    }

    val live: ZLayer[Console, Nothing, Addition] = ZLayer.fromService[Console.Service, Addition.Service] {
      (console: Console.Service) =>
        new Service {
          override def addAndPrint(x: Int): ZIO[Any, Nothing, Unit] =
            for {
              newInt <- ZIO.succeed(x + 5)
              _ <- console.putStrLn(newInt.toString).orDie
            } yield ()
        }
    }

    def addAndPrint(x: Int): ZIO[Addition, Nothing, Unit] =
      ZIO.accessM(_.get.addAndPrint(x))
  }
}
