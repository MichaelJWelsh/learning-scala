package creation

import zio.test.Assertion.{anything, equalTo, isSubtype}
import zio.test.{DefaultRunnableSpec, ZSpec, assertM}

object SuccessValuesZIOTest extends DefaultRunnableSpec {

  def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("Test succeeding ZIOs")(

    testM("s1 should be a ZIO effect that succeeds with an integer value") {
      assertM(SuccessValues.s1)(equalTo(42))
    },

    testM("s2 should be a ZIO effect that succeeds with a long value") {
      assertM(SuccessValues.s2)(isSubtype[Long](anything))
    },

    testM("s3 should be a ZIO effect that succeeds with a string value") {
      ???
    },

    testM("s4 should be a ZIO effect that succeeds with a List of String values") {
      ???
    },

    testM("s5 should be a ZIO effect that succeeds with an Option of a Boolean value") {
     ???
    }
  )

}
