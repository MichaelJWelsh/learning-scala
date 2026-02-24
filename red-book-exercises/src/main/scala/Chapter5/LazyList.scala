package Chapter5

import scala.annotation.tailrec


enum LazyList[+A] {
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  // 5.1
  def toList: List[A] = {
    /*
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
    */
    @tailrec
    def loop(ll: LazyList[A], acc: List[A]): List[A] = {
      ll match {
        case Empty => acc.reverse
        case Cons(h, t) => loop(t(), h() :: acc)
      }
    }

    loop(this, Nil)
  }

  // 5.2
  def take(n: Int): LazyList[A] = {
    this match {
      case Cons(h, t) if n > 1 => LazyList.cons(h(), t().take(n - 1))
      case Cons(h, t) if n == 1 => LazyList.cons(h(), Empty)
      case _ => Empty
    }
  }

  @tailrec
  final def drop(n: Int): LazyList[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n-1)
      case _ => this
    }
  }

  // 5.3
  def takeWhile(p: A => Boolean): LazyList[A] = {
    this match {
      case Cons(h, t) if p(h()) => LazyList.cons(h(), t().takeWhile(p))
      case _ => Empty
    }
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = {
    this.toList.length == this.takeWhile(p).toList.length
  }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = {
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _ => acc
  }

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] = {
    foldRight(Empty)((a, b) => if p(a) then LazyList.cons(a, b) else Empty)
  }

  // 5.6
  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  // 5.7
  def map[B](f: A => B): LazyList[B] = {
    foldRight(Empty: LazyList[B])((a, acc) => LazyList.cons(f(a), acc))
  }

  def filter(f: A => Boolean): LazyList[A] = {
    foldRight(Empty: LazyList[A])((a, acc) => if f(a) then LazyList.cons(a, acc) else acc)
  }

  def append[AA >: A](other: => LazyList[AA]): LazyList[AA] = {
    foldRight(other)((a, acc) => LazyList.cons(a, acc))
  }

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = {
    foldRight(Empty: LazyList[B])((a, acc) => f(a).append(acc))
  }

  // 5.8
  def continually[B](b: B): LazyList[B] = {
    LazyList.cons(b, continually(b))
  }

  // 5.9
  def from(n: Int): LazyList[Int] = {
    LazyList.cons(n, from(n + 1))
  }

  // 5.10
  def fibs: LazyList[Int] = {
    def loop(curr: Int, next: Int): LazyList[Int] = {
      LazyList.cons(curr, loop(next, curr + next))
    }

    loop(0, 1)
  }

  // 5.11
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = {
    f(state) match {
      case None => Empty
      case Some((a, s)) => LazyList.cons(a, unfold(s)(f))
    }
  }

  // 5.12
  def continuallyViaUnfold[A](a: A): LazyList[A] = {
    unfold(())(_ => Some((a, ())))
  }

  def onesViaUnfold: LazyList[Int] = {
    continuallyViaUnfold(1)
  }

  def fromViaUnfold(n: Int): LazyList[Int] = {
    unfold(n)(_ => Some((n, n+1)))
  }

  def fibsViaUnfold: LazyList[Int] = {
    unfold((0, 1)) {
      case (curr, next) => Some((curr, (next, curr + next)))
    }
  }

  // 5.13
  def mapViaUnfold[B](f: A => B): LazyList[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): LazyList[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (Empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] = {
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = {
    unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) =>
        Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) =>
        Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))
    }
  }

  // 5.14
  def startsWith[A](prefix: LazyList[A]): Boolean = {
    zipAll(prefix).takeWhile(_(1).isDefined).forAll {
      case (a1, a2) => a1 == a2
    }
  }

  // 5.15
  def tails: LazyList[LazyList[A]] = {
    unfold(this) {
      case Empty => None
      case l @ Cons(_, t) => Some((l, t()))
    }.append(LazyList(Empty))
  }
  
  // 5.16
  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] = {
    foldRight(init -> LazyList(init)) { (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1(0))
      (b2, LazyList.cons(b2, b1(1)))
    }(1)
  }
}

object LazyList {
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] = {
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))
  }
}