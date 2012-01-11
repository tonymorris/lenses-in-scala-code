import Category._

case class Lens[R, F](
  get: R => F
  , set: (R, F) => R
) {
  import Lens._

  def modify(f: F => F): R => R =
    r => set (r, f(get(r)))

  def |||[S, G](y: S @@ F): Either[R, S] @@ F =
    Lens({
      case Left(r) => get(r)
      case Right(s) => y get s
      },
      {
        case (Left(r), f) =>
          Left(set(r, f))
        case (Right(s), f) =>
          Right(y.set(s, f))
      }
    )

  def compose[Q](g: Q @@ R): Q @@ F =
    lensCat.compose(this)(g)

  def ***[S, G](y: S @@ G): (R, S) @@ (F, G) =
    Lens(
      rs => (get(rs._1), y get rs._2)
    , (rs, fg) => 
        (
          set(rs._1, fg._1)
        , y set (rs._2, fg._2)
        )
    )

  def +=(n: F)(implicit m: Numeric[F]): State[R, F] =
    State(r => {
      val w = m.plus(get(r), n)
      (w, set(r, w))
    })

  def :=(f: => F): State[R, F] =
    State(r => (f, set(r, f)))
}

object Lens {
  type @@[R, F] =
    Lens[R, F]

  def foldLens[A](x: List[A @@ A]): A @@ A =
    x.foldRight[A @@ A](lensCat.id)(_ compose _)

  def codiag[A]: Either[A, A] @@ A =
    lensCat.id ||| lensCat.id

  def first[A, B]: (A, B) @@ A =
    Lens(_._1, (ab, a) => (a, ab._2))

  def second[A, B]: (A, B) @@ B =
    Lens(_._2, (ab, b) => (ab._1, b))

  def mapL[K, V](k: K): Map[K, V] @@ Option[V] =
    Lens(
      _ get k
    , (m, v) => v match {
        case None => m - k
        case Some(w) => m + ((k, w))
      }
    )

  def setL[K](k: K): Set[K] @@ Boolean =
    Lens(
      _ contains k
    , (s, p) => if(p) s + k else s - k
    )

  implicit def st[R, F](l: R @@ F): State[R, F] =
    State(s => (l get s, s))
}


