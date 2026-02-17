import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import Chapter2.Chapter2

class Chapter2Test extends AnyFlatSpec with Matchers {
  "Fib sequence" should "be calculated correctly" in {
    Chapter2.fib(0) mustBe 1
    Chapter2.fib(1) mustBe 1
    Chapter2.fib(2) mustBe 2
    Chapter2.fib(3) mustBe 3
    Chapter2.fib(4) mustBe 5
    Chapter2.fib(5) mustBe 8
  }

  def isGt(n1: Int, n2: Int): Boolean = {
    n1 > n2
  }

  "Array" should "be sorted" in {
    Chapter2.isSorted(Array(1,2,3,4), isGt) mustBe true
    Chapter2.isSorted(Array(1,1,1,1), isGt) mustBe true
    Chapter2.isSorted(Array(1,2,3,3,4), isGt) mustBe true
    Chapter2.isSorted(Array(-2,0,5), isGt) mustBe true
    Chapter2.isSorted(Array(3,1,0), isGt) mustBe false
    Chapter2.isSorted(Array(-10,-15,-16), isGt) mustBe false
  }

  def dummyFunc(a: Int, b: Int): Int = {
    a * b
  }

  "Currying original func followed by uncurrying" should "result in original func behavior" in {
    Chapter2.uncurry(Chapter2.curry(dummyFunc))(7, 13) mustBe dummyFunc(7, 13)
    Chapter2.uncurry(Chapter2.curry(dummyFunc))(1, 2) mustBe dummyFunc(1, 2)
    Chapter2.uncurry(Chapter2.curry(dummyFunc))(-11, 21) mustBe dummyFunc(-11, 21)
    Chapter2.uncurry(Chapter2.curry(dummyFunc))(67, 80) mustBe dummyFunc(67, 80)
  }

  "Composing" should "result in combined func behavior" in {
    Chapter2.compose(
      (b: Int) => b * 7,
      (a: Int) => a * 13 + 1
    )(21) mustBe (21 * 13 + 1) * 7
  }
}