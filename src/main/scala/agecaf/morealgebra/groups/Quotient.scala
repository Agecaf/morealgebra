package agecaf.morealgebra.groups

import spire.algebra.{Eq, Group}

trait Quotient[A] extends Group[A] with Eq[A]

object Quotient {
  def apply[A : Group](equiv: (A, A) => Boolean): Quotient[A] =
    new Quotient[A] {
      def inverse(a: A) = implicitly[Group[A]].inverse(a)
      def id = implicitly[Group[A]].id
      def op(x: A, y: A) = implicitly[Group[A]].op(x, y)
      def eqv(x: A, y: A) = equiv(x, y)
    }
}