package Chapter3

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

object Tree {
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Option[Int] = t match
    case Leaf(i) => if i > 0 then Some(i) else None
    case Branch(l, r) => l.firstPositive orElse r.firstPositive

  // 3.25
  extension (t: Tree[Int]) def maximum: Int = {
    t match {
      case Leaf(n) => n
      case Branch(l, r) => l.maximum.max(r.maximum)
    }
  }

  // 3.26
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }

  // 3.27
  def map[A, B](t: Tree[A], f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }
  }

  // 3.28
  def fold[A, B](t: Tree[A], acc: (B, B) => B, f: A => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => acc(fold(l, acc, f), fold(r, acc, f))
    }
  }

  def sizeFromFold[A](t: Tree[A]): Int = {
    fold(t, 1 + _ + _, _ => 1)
  }

  extension (t: Tree[Int]) def maximumFromFold: Int = {
    fold(t, _.max(_), a => a)
  }

  def depthFromFold[A](t: Tree[A]): Int = {
    fold(t, (a1, a2) => 1 + a1.max(a2), _ => 0)
  }

  def mapFromFold[A, B](t: Tree[A], f: A => B): Tree[B] = {
    fold(t, Branch(_, _), a => Leaf(f(a)))
  }
}