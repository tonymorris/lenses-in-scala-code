case class FLens[R, F](apply: R => CoState[F, R])

