package agecaf.morealgebra.groups

import spire.algebra._

/** Semidirect Product of two Groups N, H, given the homomorphism H => End(N)
  *
  * @see [[https://en.wikipedia.org/wiki/Semidirect_product Semidirect Product (Wikipedia)]]
  *
  * =Use=
  * {{{
  *   // Minimal Imports
  *   // import agecaf.morealgebra.groups.GroupSyntax._
  *   // import agecaf.morealgebra.groups.GroupSemidirectProduct._
  *   // And whatever groups you're using.
  *
  *   // Alternatively
  *   import agecaf.morealgebra.groups._
  *
  *   {
  *     val ϕ = {(h: Double) => (n: Double) => h * n}
  *     implicit val G = SemidirectProduct(Double_+, Double_*, ϕ)
  *     // Alternatively
  *     // implicit val G = Int_+ semidirectProduct (Double_*, ϕ)
  *     // implicit val G = Int_+ ⋊ (Double_*, ϕ) // Latex `\rtimes`
  *
  *     (1.0, 2.0) |+| (3.0, 4.0)   // (1 + 2 * 3, 2 * 4) = (7.0, 6.0)
  *     (1.0, 2.0) |-| (3.0, 4.0)   // (1.0, 2.0) |+| (3.0, 4.0).inverse
  *     (1.0, 2.0).inverse          // (-0.5, 0.5)
  *     G.id                        // (0.0, 1.0)
  *   }
  * }}}
  */
trait GroupSemidirectProduct {

  /** Semidirect Product syntax sugar for Groups.
    *
    * @param N Left Hand Side Group (normal subgroup of the semidirect product).
    * @tparam N Type on which N acts on.
    */
  implicit class WithGroupSemidirectProduct[N](N: Group[N]) {

    /** See [[SemidirectProduct]]
      *
      * @param H Right Hand Side Group.
      * @tparam H Type on which H acts on.
      * @return the Semidirect Product as a Group[(N, H)].
      */
    def semidirectProduct[H] (H: Group[H], f: H => N => N): Group[(N, H)] =
      SemidirectProduct(N, H, f)

    /** See [[SemidirectProduct]]
      *
      * @param H Right Hand Side Group.
      * @tparam H Type on which H acts on.
      * @return the Semidirect Product as a Group[(N, H)].
      */
    def ⋊ [H] (H: Group[H], f: H => N => N): Group[(N, H)] = N semidirectProduct  (H, f)
  }

  /** The Direct product of two Groups.
    *
    * See [[GroupSemidirectProduct]]
    *
    * @param N Left Hand Side Group, will be the normal subgroup of the semi product.
    * @param H Right Hand Side Group.
    * @param f The homomorphism from H to End(N).
    *          This is a function {(h: H) => (n: N) => ???}.
    *          It should be an appropriate homomorphism.
    * @tparam N Type on which N acts on.
    * @tparam H Type on which H acts on.
    * @return the Semidirect Product as a Group[(N, H)].
    */
  def SemidirectProduct[N, H](N: Group[N], H: Group[H], f: H => N => N): Group[(N, H)] =
    new Group[(N, H)] {
      override def inverse(a: (N, H)): (N, H) =
        (f(H.inverse(a._2))(N.inverse(a._1)), H.inverse(a._2))


      override def id: (N, H) =
        (N.id, H.id)

      override def op(x: (N, H), y: (N, H)): (N, H) =
        (N.op(x._1, f(x._2)(y._1)), H.op(x._2, y._2))
    }
}

/** Import helper for [[GroupSemidirectProduct]]. */
object GroupSemidirectProduct extends GroupSemidirectProduct
