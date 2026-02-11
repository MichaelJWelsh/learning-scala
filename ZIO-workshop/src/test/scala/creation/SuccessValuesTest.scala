package creation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SuccessValuesTest extends AnyWordSpec with Matchers {

  val defaultRuntime: zio.Runtime[zio.ZEnv] = zio.Runtime.default

  "s1" should {
    "be a ZIO effect that succeeds with an integer value" in {
      defaultRuntime.unsafeRun(SuccessValues.s1) shouldBe a[Int]
    }
  }

  "s2" should {
    "be a ZIO effect that succeeds with a long value" in {
      defaultRuntime.unsafeRun(SuccessValues.s2) shouldBe a[Long]
    }
  }

  "s3" should {
    "be a ZIO effect that succeeds with a string value" in {
      defaultRuntime.unsafeRun(SuccessValues.s3) shouldBe a[String]
    }
  }

  "s4" should {
    "be a ZIO effect that succeeds with a List of String values" in {
      val runS4 = defaultRuntime.unsafeRun(SuccessValues.s4)
      runS4 shouldBe a[List[_]]
      runS4.foreach(_ shouldBe a [String])
    }
  }

  "s5" should {
    "be a ZIO effect that succeeds with an Option of a Boolean value" in {
      val runS5 = defaultRuntime.unsafeRun(SuccessValues.s5)
      runS5 shouldBe a[Option[_]]
      runS5.foreach(_ shouldBe a [Boolean])
    }
  }

}
