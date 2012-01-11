object LensIso {
  def ==>[R, F](x: Lens[R, F]): FLens[R, F] =
    FLens(r => CoState(x get r, x.set(r, _)))

  def <==[R, F](x: FLens[R, F]): Lens[R, F] =
    Lens(
      x apply _ get
    , x apply _ set _
    )
}

