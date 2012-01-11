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
  def coFlatten[F, R](s: CoState[F, R]): CoState[F, CoState[F, R]] =
    s coFlatMap identity
}

