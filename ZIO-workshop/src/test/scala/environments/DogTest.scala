package environments

import play.api.libs.json.{JsValue, Json}
import zio.test.Assertion.{equalTo, fails}
import zio.test._

object DogTest extends DefaultRunnableSpec {

  val bonnieJson: JsValue = Json.parse(
    """
      |{
      |  "name": "Bonnie",
      |  "age": 2,
      |  "hungry": false
      |}
      |""".stripMargin)

  val bonnie: dogs.DogInfo = dogs.DogInfo(
    "Bonnie",
    2,
    hungry = false
  )

  def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Test Dog ZIO environment")(

    testM("reading bonnie Json using the Dog Service to resolve the dependency should correctly read the dog info") {
      assertM(dogs.Dog.readDog(bonnieJson))(equalTo(bonnie)).provideSomeLayer(dogs.Dog.live)
    },

    testM("reading bonnie Json using the Always Failing Dog Service to resolve the dependency should fail with an IncorrectDogService error") {
      assertM(dogs.Dog.readDog(bonnieJson).run)(fails(equalTo(dogs.IncorrectDogService))).provideSomeLayer(dogs.Dog.alwaysFailingLive)
    }
  )

}
