package Chapter2

import scala.annotation.tailrec

object Chapter2 {
  // 2.1
  def fib(n: Int): Int = {
    @tailrec
    def fibHelper(currN: Int, goalN: Int, prevPrevNum: Int, prevNum: Int): Int = {
      if (currN == goalN) {
        prevPrevNum + prevNum
      }
      else {
        var newNum: Int = prevPrevNum + prevNum
        fibHelper(currN + 1, goalN, prevNum, newNum)
      }
    }

    n match {
      case 0 => 1
      case 1 => 1
      case _ => fibHelper(2, n, 1, 1)
    }
  }

  // 2.2
  def isSorted[A](arr: Array[A], isGt: (A, A) => Boolean): Boolean = {
    @tailrec
    def isSortedHelper(currN: Int): Boolean = {
      if (currN + 1 >= arr.length) {
        true
      }
      else if (isGt(arr(currN), arr(currN + 1))) {
        false
      }
      else {
        isSortedHelper(currN + 1)
      }
    }

    isSortedHelper(0)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}