trait Category[~>[_, _]] {
  def compose[A, B, C](f: B ~> C)(g: A ~> B): A ~> C
  def id[A]: A ~> A
}

object Category {
  val lensCat =
    new Category[Lens] {
      def compose[A, B, C](f: Lens[B, C])(g: Lens[A, B]) =
        Lens(
          get = f.get compose g.get
        , set = (a, c) => g set(a, f set (g get a, c))
        )
      def id[A]: Lens[A, A] =
        Lens(
          get = identity
        , set = (_, a) => a
        )
    }
}
