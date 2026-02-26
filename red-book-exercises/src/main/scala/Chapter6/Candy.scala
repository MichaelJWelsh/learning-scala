package Chapter6

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  import State.*  // brings unit/sequence + extension methods into scope

  private def step(i: Input)(m: Machine): Machine =
    if m.candies == 0 then m
    else (i, m.locked) match
      case (Input.Coin, true)  => m.copy(locked = false, coins = m.coins + 1)
      case (Input.Turn, false) => m.copy(locked = true,  candies = m.candies - 1)
      case _                   => m

  private val get: State[Machine, Machine] =
    State(m => (m, m))

  private def set(m: Machine): State[Machine, Unit] =
    State(_ => ((), m))

  private def modify(f: Machine => Machine): State[Machine, Unit] =
    get.flatMap(m => set(f(m)))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(inputs.map(i => modify(step(i))))
      .flatMap(_ => get.map(m => (m.coins, m.candies)))
