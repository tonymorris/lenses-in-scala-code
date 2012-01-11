case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, t) = run(s)
      (f(a), t)
    })

  def flatMap[B]
    (f: A => State[S, B])
    : State[S, B] =
      State(s => {
        val (a, t) = run(s)
        f(a) run t
      })

  def eval(s: S): A =
    run(s)._1
}

object State {
  def get[S]: State[S, S] =
    State(s => (s, s))
}

