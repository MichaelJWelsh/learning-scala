package changing

import changing.ChangingZios.{DynamicError, Error1}
import zio.test.Assertion.{anything, equalTo, fails, isSubtype, succeeds}
import zio.test.{DefaultRunnableSpec, ZSpec, assertM}

object ChangingZiosTest extends DefaultRunnableSpec {

  def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Test changing ZIOs")(

    testM("s2 should be a ZIO effect that succeeds with 42 mapped to a string value") {
      assertM(ChangingZios.s2)(equalTo("42"))
    },

    testM("i2 should be a ZIO effect that fails with a NumberFormatException") {
      assertM(ChangingZios.i2.run)(fails(isSubtype[NumberFormatException](anything)))
    },

    testM("f2 should be a ZIO effect that fails with an Exception") {
      assertM(ChangingZios.f2.run)(fails(isSubtype[Exception](anything)))
    },

    testM("s4 should be a ZIO effect that succeeds with 42 mapped to a double value") {
      assertM(ChangingZios.s4.run)(succeeds(equalTo(42.0)))
    },

    testM("i5 should be a ZIO effect that fails with a NumberFormatException") {
      assertM(ChangingZios.i5.run)(fails(isSubtype[NumberFormatException](anything)))
    },

    testM("i6 should be a ZIO effect that succeeds with 42 mapped to a double") {
      assertM(ChangingZios.i6.run)(succeeds(equalTo(42.0)))
    },

    testM("f4 should be a ZIO effect that fails with a DynamicError") {
      assertM(ChangingZios.f4.run)(fails(isSubtype[ChangingZios.DynamicError](anything)))
    },

    testM("f5 should be a ZIO effect that fails with Error1") {
      assertM(ChangingZios.f5.run)(fails(isSubtype[ChangingZios.Error](anything)))
    }
  )

}
