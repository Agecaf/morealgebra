package agecaf.morealgebra.groups

import spire.algebra.Group

trait CosetSyntax {
  implicit class GroupWithCosetSyntax[A](G: Group[A]) {
    def / (H: Subgroup[A]): Quotient[A] =
      Quotient[A]{ (x, y) =>
        H contains G.op(x, G.inverse(y))
      }
  }
}