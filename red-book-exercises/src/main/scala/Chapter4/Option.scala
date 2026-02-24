package Chapter4

// 4.1
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(get) => Some(f(get))
      case None() => None()
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case Some(get) => f(get)
      case None() => None()
    }
  }

  def getOrElse[B >: A](lazyVal: => B): B = {
    this match {
      case Some(get) => get
      case None() => lazyVal
    }
  }

  def orElse[B >: A](lazyOption: => Option[B]): Option[B] = {
    this match {
      case Some(_) => this
      case None() => lazyOption
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(get) if f(get) => this
      case _ => None()
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case class None() extends Option[Nothing]

object Option {
  // 4.2
  def mean(xs: Seq[Double]): Option[Double] = {
    if xs.isEmpty then None()
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m.flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (_, None()) => None()
      case (None(), _) => None()
      case (Some(get1), Some(get2)) => Some(f(get1, get2))
    }
  }

  // 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))
  }

  // 4.5
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))
  }

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = {
    traverse(as)(a => a)
  }
}
