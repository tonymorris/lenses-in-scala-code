import org.scalacheck._, Prop._

object LensIsoCheck extends Properties("LensIso") {
  property("Lens and FLens is isomorphic") = 
    forAll((r: Double, f: Double) =>
      (r >= 0 && f >= 0) ==> {
        val p = math.pow(_: Double, _: Double)
        val canonical = p(f, r) * p(r, f * r)
        val alternative = p(f * p(r, f), r)
        canonical == alternative
        }
      )
}

