package agecaf.morealgebra.groups

import spire.algebra.Group

object Kernel {
  def apply[A, B](f: A => B)(implicit domain: Group[A], range: Group[B]): Subgroup[A] = {
    implicit val group = domain
    Subgroup[A]{ a => f(a) == range.id }
  }
}

