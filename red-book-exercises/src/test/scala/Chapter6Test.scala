import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import Chapter6.SimpleRNG
import Chapter6.Rand
import Chapter6.RNG
import Chapter6.State
import Chapter6.{Candy, Machine}
import Chapter6.Input.{Coin, Turn}
import Chapter6.Chapter6

class Chapter6Test extends AnyFlatSpec with Matchers {
  "Exercise 6.1" should "properly implement nonNegativeInt" in {
    val (i, r) = Chapter6.nonNegativeInt(SimpleRNG(0))
    val (i2, r2) = Chapter6.nonNegativeInt(SimpleRNG(0))
    i mustBe i2
    r mustBe r2
    i >= 0 mustBe true
  }

  "Exercise 6.2" should "properly implement double" in {
    val (i, r) = Chapter6.double(SimpleRNG(0))
    val (i2, r2) = Chapter6.double(SimpleRNG(0))
    i mustBe i2
    r mustBe r2
    i >= 0 mustBe true
    i < 1 mustBe true
  }

  "Exercise 6.3" should "properly implement intDouble, doubleInt, double3" in {
    val ((i, d), _) = Chapter6.intDouble(SimpleRNG(0))
    i.isInstanceOf[Int] mustBe true
    d.isInstanceOf[Double] mustBe true
    val ((d2, i2), _) = Chapter6.doubleInt(SimpleRNG(0))
    i2.isInstanceOf[Int] mustBe true
    d2.isInstanceOf[Double] mustBe true
    val ((d3, d4, d5), _) = Chapter6.double3(SimpleRNG(0))
    d3.isInstanceOf[Double] mustBe true
    d4.isInstanceOf[Double] mustBe true
    d5.isInstanceOf[Double] mustBe true
  }

  "Exercise 6.4" should "properly implement ints" in {
    val (l, r) = Chapter6.ints(5)(SimpleRNG(0))
    l.length mustBe 5
  }

  "Exercise 6.5" should "properly implement doubleViaMap" in {
    val (i, r) = Chapter6.double(SimpleRNG(0))
    val (i2, r2) = Chapter6.doubleViaMap(SimpleRNG(0))
    i mustBe i2
    r mustBe r2
    i >= 0 mustBe true
    i < 1 mustBe true
  }

  "Exercise 6.6" should "properly implement map2" in {
    val rng0 = SimpleRNG(0)
    val ra: Rand[Int] = _.nextInt
    val rb: Rand[Int] = _.nextInt
    val combined: Rand[Int] = Chapter6.map2(ra, rb)(_ + _)
    val (sum, rngAfter) = combined(rng0)
    val (a, rng1) = ra(rng0)
    val (b, rng2) = rb(rng1)
    sum mustBe (a + b)
    rngAfter mustBe rng2
  }

  "Exercise 6.7" should "properly implement sequence, intsViaSequence" in {
    val rng = SimpleRNG(0)
    val rands: List[Rand[Int]] = List(_.nextInt, _.nextInt, _.nextInt)
    val (seqList, rngAfterSeq) = Chapter6.sequence(rands)(rng)
    seqList.length mustBe 3
    seqList.forall(_.isInstanceOf[Int]) mustBe true
    rngAfterSeq.isInstanceOf[RNG] mustBe true
    val (intsList, rngAfterInts) = Chapter6.intsViaSequence(5)(rng)
    intsList.length mustBe 5
    intsList.forall(_.isInstanceOf[Int]) mustBe true
    rngAfterInts.isInstanceOf[RNG] mustBe true
  }

  "Exercise 6.8" should "properly implement flatMap" in {
    val rng = SimpleRNG(0)
    val ra: Rand[Int] = _.nextInt
    val doubled: Rand[Int] = Chapter6.flatMap(ra)(a => Chapter6.unit(a * 2))
    val (result, rngAfter) = doubled(rng)
    val (a, rng1) = ra(rng)
    val expected = a * 2
    result mustBe expected
    rngAfter mustBe rng1
  }

  "Exercise 6.9" should "properly implement mapViaFlatMap" in {
    val rng = SimpleRNG(0)
    val r: Rand[Int] = _.nextInt
    val (v1, r1) = Chapter6.mapViaFlatMap(r)(_ * 2)(rng)
    val (v2, r2) = Chapter6.map(r)(_ * 2)(rng)
    v1 mustBe v2
    r1 mustBe r2
    val ra: Rand[Int] = _.nextInt
    val rb: Rand[Int] = _.nextInt
    val (vv1, rr1) = Chapter6.map2ViaFlapMap(ra, rb)(_ + _)(rng)
    val (vv2, rr2) = Chapter6.map2(ra, rb)(_ + _)(rng)
    vv1 mustBe vv2
    rr1 mustBe rr2
  }

  
  "Exercise 6.10" should "properly implement State unit/map/flatMap/map2/sequence" in {
    val s = State[Int, Int](n => (n, n + 1))
    val prog =
      s.map(_ + 1)
        .flatMap(a => State.unit(a * 2))
        .map2(State.unit(10))(_ + _)
    val (v1, st1) = prog.run(0)
    val (v2, st2) = prog.run(0)
    v1 mustBe v2
    st1 mustBe st2
    val (list, _) = State.sequence(List(State.unit(1), State.unit(2), State.unit(3))).run(0)
    list mustBe List(1, 2, 3)
  }

  "Exercise 6.11" should "properly simulate the candy machine" in {
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val initial = Machine(locked = true, candies = 5, coins = 10)
    val ((coins, candies), _) = Candy.simulateMachine(inputs).run(initial)
    coins mustBe 14
    candies mustBe 1
  }
}