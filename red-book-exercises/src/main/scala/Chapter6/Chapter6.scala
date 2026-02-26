package Chapter6

import scala.annotation.tailrec

type Rand[+A] = RNG => (A, RNG)

sealed trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Chapter6 {
  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    if (nextInt < 0) {
      (-1 * (nextInt + 1), nextRng)
    }
    else {
      (nextInt, nextRng)
    }
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(currN: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (currN == count) {
        (acc, r)
      }
      else {
        val (i, r2) = r.nextInt
        loop(currN + 1, r2, i :: acc)
      }
    }

    loop(0, rng, Nil)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)
  }

  // 6.5
  val doubleViaMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng0 => {
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }
  }

  def unit[A] (a: A): Rand[A] = {
    rng => (a, rng)
  }

  val int: Rand[Int] = _.nextInt

  // 6.7
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }
  
  // 6.8
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = {
    r0 => {
      val (a, r1) = r(r0)
      f(a)(r1)
    }
  }
  
  // 6.9
  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = {
    flatMap(r)(a => unit(f(a)))
  }

  def map2ViaFlapMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
  
}

