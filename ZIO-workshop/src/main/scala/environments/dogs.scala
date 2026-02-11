package environments

import play.api.libs.json.{Format, JsValue, Json}
import zio.{Has, ZIO, ZLayer}


// create a Dog Service that can resolve json to DogInfo
// it should come with 2 implementations, one that always fails, one that actually reads the json

object dogs {
  type Dog = Has[Dog.Service]

  case class DogInfo(name: String, age: Int, hungry: Boolean)

  object DogInfo {
    implicit val format: Format[DogInfo] = Json.format[DogInfo]
  }

  trait DogReadError

  case object IncorrectJson extends DogReadError

  case object IncorrectDogService extends DogReadError

  object Dog {
    trait Service {
      def readDog(json: JsValue): ZIO[Any, DogReadError, DogInfo]
    }

    val live: ZLayer[Any, DogReadError, Dog] = ZLayer.succeed {
      new Service {
        override def readDog(json: JsValue): ZIO[Any, DogReadError, DogInfo] =
          ZIO.fromOption(json.asOpt[DogInfo]).mapError(_ => IncorrectJson)
      }
    }

    val alwaysFailingLive: ZLayer[Any, DogReadError, Dog] = ZLayer.succeed {
      new Service {
        override def readDog(json: JsValue): ZIO[Any, DogReadError, DogInfo] =
          ZIO.fail(IncorrectDogService)
      }
    }

    def readDog(json: JsValue): ZIO[Dog, DogReadError, DogInfo] =
      ZIO.accessM(_.get.readDog(json))
  }
}
