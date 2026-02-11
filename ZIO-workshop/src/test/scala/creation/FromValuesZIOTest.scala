package creation

import creation.FromValues.Dog
import creation.FromValues.Dog.DogReadError
import zio.ZIO
import zio.test.Assertion.{anything, equalTo, fails, isSubtype, succeeds}
import zio.test.{DefaultRunnableSpec, ZSpec, assertM}

object FromValuesZIOTest extends DefaultRunnableSpec {

  def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Check ZIOs from other values")(

    testM("zOpt3 should be a ZIO effect that succeeds with an integer of 2") {
      assertM(FromValues.zOpt3)(equalTo(2))
    },

    testM("zOpt3 should be a ZIO effect that fails with a string") {
      assertM(FromValues.zOpt4.run)(fails(isSubtype[String](anything)))
    },

    testM("zEither1 should be a ZIO effect that succeeds with an integer of 5") {
      assertM(FromValues.zEither1)(equalTo(5))
    },

    testM("zEither2 should be a ZIO effect that fails with a string") {
      assertM(FromValues.zEither2.run)(fails(isSubtype[String](anything)))
    },

    testM("s4 should be a ZIO effect that succeeds with an integer value of 21") {
      assertM(FromValues.zTry1)(equalTo(21))
    },

    testM("zTry2 should be a ZIO effect that fails with a ClassCastException") {
      assertM(FromValues.zTry2.run)(fails(isSubtype[ClassCastException](anything)))
    },

    testM("zioBonnie should be a ZIO effect that succeeds with a Dog") {
      assertM(FromValues.zioBonnie.run)(succeeds(isSubtype[FromValues.Dog](anything)))
    },

    testM("zioBunny should be a ZIO effect that fails with a DogReadError") {
      assertM(FromValues.zioBunny.run)(fails(isSubtype[FromValues.Dog.DogReadError](anything)))
    }
  )

}
