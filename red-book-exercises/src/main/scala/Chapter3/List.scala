package Chapter3

import scala.annotation.tailrec

enum List[+A] {
  case Nil
  case Cons(head: A, tail: List[A])
}

object List {
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def product(ds: List[Double]): Double = {
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
  }

  def apply[A](as: A*): List[A] = {
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))
  }

  // 3.1
  val x: Int = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 1
  }

  // 3.2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, t) => t
      case Nil => Nil
    }
  }

  // 3.3
  def setHead[A](l: List[A], newVal: A): List[A] = {
    l match {
      case Cons(_, t) => Cons(newVal, t)
      case Nil => Cons(newVal, Nil)
    }
  }

  // 3.4
  def drop[A](as: List[A], n: Int): List[A] = {
    @tailrec
    def loop(currAs: List[A], currN: Int): List[A] = {
      if (currN < n) {
        loop(tail(currAs), currN + 1)
      }
      else {
        currAs
      }
    }

    loop(as, 0)
  }

  // 3.5
  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = {
    as match {
      case Cons(h, t) =>
        if (f(h)) {
          dropWhile(t, f)
        }
        else {
          as
        }
      case Nil => as
    }
  }

  // 3.6
  def init[A](as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]) = foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]) = foldRight(ns, 1.0, _ * _)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))

  // 3.7
  // No, f cannot be immediately halted if it encounters a 0.0
  // because it isn't invoked until the entire list has been traversed recursively

  // 3.8
  val y: List[Int] = foldRight(List(1,2,3), Nil: List[Int], Cons(_, _))

  // 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0, (_, acc) => acc + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = {
    as match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)
    }
  }

  // 3.11
  def sumViaFoldLeft(ns: List[Int]) = foldLeft(ns, 0, (y, x) => x + y)
  def productViaFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0, _ * _)
  def lengthViaFoldLeft[A](as: List[A]): Int = foldLeft(as, 0, (acc, _) => acc + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A], (b, a) => Cons(a, b))

  // 3.13
  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f: (A, B) => B): B = {
    foldLeft(reverse(as), acc, (b, a) => f(a, b))
  }

  def foldLeftViaFoldRight[A, B](as: List[A], acc: B, f: (B, A) => B): B = {
    foldRight(reverse(as), acc, (a, b) => f(b, a))
  }

  // 3.14
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2, (b, a) => Cons(a, b))
  }

  // 3.15
  def flatten[A](ll: List[List[A]]): List[A] = {
    foldRight(ll, Nil: List[A], appendViaFoldLeft)
  }

  // 3.16
  def plusOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int], (a, acc) => Cons(a + 1, acc))
  }

  // 3.17
  def listOfDoubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String], (a, acc) => Cons(a.toString, acc))
  }

  // 3.18
  def map[A, B](as: List[A], f: A => B): List[B] = {
    foldRight(as, Nil: List[B], (a, acc) => Cons(f(a), acc))
  }

  // 3.19
  def filter[A](as: List[A], f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A], (a, acc) => if f(a) then Cons(a, acc) else acc)
  }

  // 3.20
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = {
    flatten(map(as, f))
  }

  // 3.21
  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = {
    flatMap(as, a => if f(a) then List(a) else Nil)
  }

  // 3.22
  def addLists(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }
  }

  // 3.23
  def addListsGeneric[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), addListsGeneric(t1, t2, f))
    }
  }

  // 3.24
  @tailrec
  def startsWith[A](l: List[A], startL: List[A]): Boolean = {
    (l, startL) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) then startsWith(t1, t2) else false
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
      case (Cons(_, t1), _) => if (startsWith(sup, sub)) then true else hasSubsequence(t1, sub)
    }
  }
}
