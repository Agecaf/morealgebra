package agecaf.morealgebra.sets

import spire.algebra.Eq

trait Reduce[A] extends Eq[A]  {
  def reduce(x: A): A
  override def eqv(a: A, b: A): Boolean =
    reduce(a) == reduce(b)
}

trait ReduceSyntax {
  implicit class WithReduceSyntax[A : Reduce](x: A) {
    def reduce = implicitly[Reduce[A]].reduce(x)
  }
}