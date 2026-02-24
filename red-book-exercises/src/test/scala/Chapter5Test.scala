import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import Chapter5.LazyList

class Chapter5Test extends AnyFlatSpec with Matchers {
  "Exercise 5.1" should "properly implement toList" in {
    LazyList(1,2,3).toList mustBe List(1,2,3)
  }

  "Exercise 5.2" should "properly implement take and drop" in {
    LazyList(1, 2, 3).take(2).toList mustBe List(1, 2)
    LazyList(1, 2, 3).drop(2).toList mustBe List(3)
  }

  "Exercise 5.3" should "properly implement takeWhile" in {
    LazyList(2, 4, 5, 6).takeWhile(_ % 2 == 0).toList mustBe List(2, 4)
  }

  "Exercise 5.4" should "properly implement forAll" in {
    LazyList(2, 4, 6, 8).forAll(_ % 2 == 0) mustBe true
    LazyList(2, 4, 5, 8).forAll(_ % 2 == 0) mustBe false
  }

  "Exercise 5.5" should "properly implement takeWhileViaFoldRight" in {
    LazyList(2, 4, 5, 6).takeWhile(_ % 2 == 0).toList mustBe LazyList(2, 4, 5, 6).takeWhileViaFoldRight(_ % 2 == 0).toList
  }

  "Exercise 5.6" should "properly implement headOption" in {
    LazyList(2, 4, 6, 8).headOption mustBe Some(2)
  }

  "Exercise 5.7" should "properly implement map/filter/append/flatMap using foldRight" in {
    LazyList(1,2,3,4).map(_.toString).toList mustBe List("1", "2", "3", "4")
    LazyList(2, 4, 5, 6, 8).filter(_ % 2 == 0).toList mustBe List(2,4,6,8)
    LazyList(1,2,3,4).append(LazyList(5,6,7,8)).toList mustBe List(1,2,3,4,5,6,7,8)
    LazyList(1,2,3,4).flatMap(x => LazyList.cons(x.toString, LazyList.Empty)).toList mustBe List("1", "2", "3", "4")
  }

  "Exercise 5.11" should "properly implement unfold" in {
    LazyList().unfold(1)(s => if s <= 5 then Some((s, s + 1)) else None).toList mustBe List(1,2,3,4,5)
  }

  "Exercise 5.13" should "properly implement map/take/takeWhile/zipWith using unfold" in {
    LazyList(1, 2, 3, 4).mapViaUnfold(_.toString).toList mustBe LazyList(1, 2, 3, 4).map(_.toString).toList
    LazyList(1, 2, 3).takeViaUnfold(2).toList mustBe LazyList(1, 2, 3).take(2).toList
    LazyList(2, 4, 5, 6).takeWhileViaUnfold(_ % 2 == 0).toList mustBe LazyList(2, 4, 5, 6).takeWhile(_ % 2 == 0).toList
    LazyList(1,2,3).zipAll(LazyList(4,5,6)).toList mustBe List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)))
  }

  "Exercise 5.14" should "properly implement startsWith" in {
    LazyList(1,2,3,4).startsWith(LazyList(1,2)) mustBe true
  }

  "Exercise 5.15" should "properly implement tails" in {
    LazyList(1,2,3,4).tails.map(_.toList).toList mustBe
      List(
        List(1,2,3,4),
        List(2,3,4),
        List(3,4),
        List(4),
        List()
      )
  }

  "Exercise 5.16" should "properly implement scanRight" in {
    LazyList(1,2,3).scanRight(0)(_ + _).toList mustBe List(6,5,3,0)
  }
}