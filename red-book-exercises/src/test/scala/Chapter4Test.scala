import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import Chapter4.Option
import Chapter4.{Some, None}
import Chapter4.Either
import Chapter4.{Left, Right}

class Chapter4Test extends AnyFlatSpec with Matchers {
  "Exercise 4.1" should "properly implement Option" in {
    Some(5).map(x => x * 2.5) mustBe Some(12.5)
    Some(3).flatMap(x => None()) mustBe None()
    Some(3).flatMap(x => None()).getOrElse(Some("wow")) mustBe Some("wow")
    Some(3).flatMap(x => None()).orElse(Some("wow")) mustBe Some("wow")
    Some(3).filter(x => x % 2 == 0) mustBe None()
  }

  "Exercise 4.2" should "properly implement variance" in {
    Option.variance(List(1,1,1,1,1)) mustBe Some(0.0)
  }

  "Exercise 4.3" should "properly implement map2" in {
    Option.map2(Some(10), Some(2.5))(_ * _) mustBe Some(25)
    Option.map2(None(): Option[Double], Some(2.5))(_ * _) mustBe None()
  }

  "Exercise 4.4" should "properly implement sequence" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) mustBe Some(List(1,2,3))
    Option.sequence(List(Some(1), None(), Some(3))) mustBe None()
  }

  "Exercise 4.5" should "properly implement traverse and sequenceViaTraverse" in {
    Option.traverse[String, Int](List("1", "2", "3"))(x => Some(x.toInt)) mustBe Some(List(1, 2, 3))
    Option.sequence(List(Some(1), Some(2), Some(3))) mustBe Option.sequenceViaTraverse(List(Some(1), Some(2), Some(3)))
  }

  "Exercise 4.6" should "properly implement Either" in {
    Right(5).map(x => x * 2.5) mustBe Right(12.5)
    Right(3).flatMap(x => Left("yikes")) mustBe Left("yikes")
    Right(3).flatMap(x => Left("yikes")).orElse(Right("wow")) mustBe Right("wow")
    Right(1).map2(Right(2))(_ + _) mustBe Right(3)
  }

  "Exercise 4.7" should "properly implement Either's traverse and sequence" in {
    Either.traverse[Nothing, String, Int](List("1", "2", "3"))(x => Right(x.toInt)) mustBe Right(List(1, 2, 3))
    Either.sequence(List(Right(1), Right(2), Right(3))) mustBe Right(List(1,2,3))
  }
}