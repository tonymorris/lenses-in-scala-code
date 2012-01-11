case class CoState[F, R](
  get: F
, set: F => R
) {
  def map[S](f: R => S): CoState[F, S] =
    CoState(
      get
    , f compose set
    )

  def coFlatMap[S]
    (f: CoState[F, R] => S)
    : CoState[F, S] =
      CoState(
        get
      , k => f(CoState(k, set))
      )
}

object CoState {
  def extract[F, R](s: CoState[F, CoState[F, R]]): CoState[F, R] =
    CoState(s.get, k => s set k set k)
}

