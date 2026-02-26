// 6.10

package Chapter6

opaque type State[S, +A] = S => (A, S)

object State:
  def apply[S, A](f: S => (A, S)): State[S, A] = f
  
  def unit[S, A](a: A): State[S, A] =
    s => (a, s)
  
  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(unit[S, List[A]](Nil))((r, acc) => r.map2(acc)(_ :: _))

  extension [S, A](sa: State[S, A])
    def run(s: S): (A, S) = sa(s)
    
    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, s1) = sa(s)
        (f(a), s1)
    
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = sa(s)
        f(a)(s1)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      sa.flatMap(a => sb.map(b => f(a, b)))
