package agecaf.morealgebra.groups

import spire.algebra._

/** Direct Product of two Groups.
  *
  * @see [[https://en.wikipedia.org/wiki/Direct_product_of_groups Direct Product (Groups)]]
  *
  * =Use=
  *
  * {{{
  *   // Minimal Imports
  *   // import agecaf.morealgebra.groups.GroupSyntax._
  *   // import agecaf.morealgebra.groups.GroupDirectProduct._
  *   // And whatever groups you're using.
  *
  *   // Alternatively
  *   import agecaf.morealgebra.groups._
  *
  *   {
  *     implicit val G = DirectProduct(Int_+, Double_*)
  *     // Alternatively
  *     // implicit val G = Int_+ directProduct Double_*
  *     // implicit val G = Int_+ × Double_* // Latex `\times`, not `x`
  *
  *     (1, 2.0) |+| (-2, 3.0)   // (-1, 6.0)
  *     (1, 2.0) |-| (-2, 4.0)   // (3, 0.5)
  *     (1, 2.0).inverse         // (-1, 0.5)
  *     G.id                     // (0, 1.0)
  *   }
  * }}}
  */
trait GroupDirectProduct {

  /** Direct Product syntax sugar for Groups.
    *
    * @param lhs Left Hand Side Group.
    * @tparam A Type on which lhs acts on.
    */
  implicit class WithGroupDirectProduct[A](lhs: Group[A]) {

    /** See [[DirectProduct]]
      *
      * @param rhs Right Hand Side Group.
      * @tparam B Type on which rhs acts on.
      * @return the Direct Product as a Group[(A, B)].
      */
    def directProduct[B] (rhs: Group[B]): Group[(A, B)] =
      DirectProduct(lhs, rhs)

    /** See [[DirectProduct]]
      *
      * @param rhs Right Hand Side Group.
      * @tparam B Type on which rhs acts on.
      * @return the Direct Product as a Group[(A, B)].
      */
    def × [B] (rhs: Group[B]): Group[(A, B)] = lhs directProduct  rhs
  }

  /** The Direct product of two Groups.
    *
    * See [[GroupDirectProduct]]
    *
    * @param lhs Left Hand Side Group.
    * @param rhs Right Hand Side Group.
    * @tparam A Type on which lhs acts on.
    * @tparam B Type on which rhs acts on.
    * @return the Direct Product as a Group[(A, B)].
    */
  def DirectProduct[A, B](lhs: Group[A], rhs: Group[B]): Group[(A, B)] =
    new Group[(A, B)] {

      override def inverse(a: (A, B)): (A, B) =
        (lhs.inverse(a._1), rhs.inverse(a._2))

      override def id: (A, B) =
        (lhs.id, rhs.id)

      override def op(x: (A, B), y: (A, B)): (A, B) =
        (lhs.op(x._1, y._1), rhs.op(x._2, y._2))
    }
}

/** Import helper for [[GroupDirectProduct]]. */
object GroupDirectProduct extends GroupDirectProduct
