package agecaf.morealgebra.groups

import agecaf.morealgebra.sets.Reduce
import spire.algebra.Group

trait ReducibleQuotient[A] extends Reduce[A] with Quotient[A]

object ReducibleQuotient {
  def apply[A : Group](_reduce: A => A): ReducibleQuotient[A] =
    new ReducibleQuotient[A] {
      def inverse(a: A) = reduce( implicitly[Group[A]].inverse(a) )
      def id = implicitly[Group[A]].id  // ID should be in reduced state.
      def op(x: A, y: A) = reduce(implicitly[Group[A]].op(x, y))
      def reduce(x: A) = _reduce(x)
    }
}
