package agecaf.morealgebra.groups

import spire.algebra.Group


object Image {
  def apply[A, B](f: A => B)(implicit domain: Group[A], range: Group[B]): Quotient[A] = {
    implicit val group = domain
    Quotient[A]{ (x, y) => f(x) == f(y) }
  }
}
