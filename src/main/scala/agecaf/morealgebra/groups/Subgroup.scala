package agecaf.morealgebra.groups

import agecaf.morealgebra.sets.{Cardinality, Finite, Subset}
import spire.algebra.Group

trait Subgroup[A] extends Group[A] with Cardinality[A] {

  val set: Subset[A]

  def unapply(arg: A): Option[A] = set.unapply(arg)
  def contains(x: A) = set contains x
}

object Subgroup {
  def apply[A](finiteSet: Set[A])(implicit group: Group[A]): Subgroup[A] with Finite[A] = {
    new Subgroup[A] with Finite[A]{
      override val set = Subset(finiteSet)(this)
      def op(x: A, y: A) = group.op(x, y)
      val everything = finiteSet
      def inverse(a: A) = group.inverse(a)
      def id = group.id
    }
  }

  def apply[A](condition: A => Boolean)(implicit group: Group[A], card: Cardinality[A]): Subgroup[A] = {
    new Subgroup[A] {
      override val set = Subset(condition)(this)
      def op(x: A, y: A) = group.op(x, y)
      lazy val everythingOpt = card.everythingOpt map (_ filter condition)
      lazy val generatorOpt = card.generatorOpt map (_ filter condition)
      def inverse(a: A) = group.inverse(a)
      def id = group.id
    }
  }
}
