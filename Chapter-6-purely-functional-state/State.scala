opaque type State[S, +A] = S => (A, S)

object State:
  extension [S,A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S,A](a: A): State[S, A] = (s: S) => (a, s)

  def map[S, A, B](action: State[S, A])(f: A => B): State[S, B] =
    state =>
      val (a, nextState) = action(state)
      (f(a), nextState)

  def map2[S, A, B, C](action1: State[S, A], action2: State[S, B])(f: (A, B) => C): State[S, C] =
    state =>
      val (a, nextState) = action1(state)
      val (b, nextNextState) = action2(nextState)
      (f(a, b), nextNextState)

  def flatMap[S, A, B](action: State[S, A])(f: A => State[S, B]): State[S, B] =
    state =>
      val (a, nextState) = action(state)
      f(a)(nextState)
