package creation

import creation.FromValues.Dog.{DogReadError, IncorrectJson}
import play.api.libs.json.{Format, JsValue, Json}
import zio.ZIO

import scala.util.Try


object FromValues {

  private val someInt: Option[Int] = Some(2)

  //Examples from workshop
  val zOpt1: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(someInt)
  val zOpt2: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(None)
  val zOpt3: ZIO[Any, String, Int] = zOpt1.mapError(_ => "There was no integer")
  val zOpt4: ZIO[Any, String, Int] = zOpt2.mapError(_ => "There was no integer")

  val maybeInt: Either[String, Int] = Right(5)
  val maybeInt2: Either[String, Int] = Left("Not an integer!")

  val zEither1: ZIO[Any, String, Int] = ZIO.fromEither(maybeInt)
  val zEither2: ZIO[Any, String, Int] = ZIO.fromEither(maybeInt2)

  val zTry1: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42 / 2))
  val zTry2: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try("hello".asInstanceOf[Int]))


  //Working examples

  case class Dog(name: String, age: Int, hungry: Boolean)

  object Dog {
    implicit val format: Format[Dog] = Json.format[Dog]

    trait DogReadError
    case object IncorrectJson extends DogReadError
  }

  val bonnie: JsValue = Json.parse(
    """
      |{
      |  "name": "Bonnie",
      |  "age": 2,
      |  "hungry": false
      |}
      |""".stripMargin)

  val notBonnie: JsValue =  Json.parse(
    """
      |{
      |  "name": "Bunny",
      |  "age": 1,
      |  "hungry": "always",
      |  "not a dog key": "val"
      |}
      |""".stripMargin)

  val maybeBonnie1: Option[Dog] = bonnie.asOpt[Dog]
  val mabyeBonnie2: Option[Dog] = notBonnie.asOpt[Dog]


  //map the JsValues above to ZIOs of type Dog, failures should move to the error type
  val zioBonnie: ZIO[Any, DogReadError, Dog] = ZIO.fromOption(maybeBonnie1).mapError(_ => IncorrectJson)
  val zioBunny: ZIO[Any, DogReadError, Dog] = ZIO.fromOption(mabyeBonnie2).mapError(_ => IncorrectJson)

}
