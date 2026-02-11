package creation

import zio.test.Assertion.{anything, equalTo, fails, isSubtype}
import zio.test.{DefaultRunnableSpec, ZSpec, assertM}

object FailureValuesTest extends DefaultRunnableSpec {

  def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Test failing ZIOs")(

    testM("Failure with String value") {

      assertM(FailureValues.f1.run)(fails(equalTo("Boom!")))

    },

    testM("Failure with Throwable") {

      assertM(FailureValues.f2.run)(fails(isSubtype[Exception](anything)))

    },

    testM("Failure with int value") {
      assertM(FailureValues.f3.run)(fails(isSubtype[Int](anything)))
    },

    testM("Failure with Error1") {
      assertM(FailureValues.f4.run)(fails(equalTo(FailureValues.Error1)))
    }
  )
}
