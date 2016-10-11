package agecaf.morealgebra.groups

import agecaf.morealgebra.sets.{Cardinality, Finite, Subset}
import spire.algebra.Group

trait Subgroup[A] extends Group[A] with Cardinality[A] {

  val set: Subset[A]

  def unapply(arg: A): Option[A] = set.unapply(arg)
  def contains(x: A) = set contains x

  def & (other: Subgroup[A]): Subgroup[A] = {
    val parent = this
    new Subgroup[A] {
      val set: Subset[A] = parent.set & other.set
      def inverse(a: A) = parent.inverse(a)
      def id = parent.id
      def op(x: A, y: A) = parent.op(x, y)
      val everythingOpt: Option[Set[A]] =
        (parent.everythingOpt, other.everythingOpt) match {
          case (Some(s1), Some(s2)) => Some(s1 & s2)
          case _ => None
        }
      val generatorOpt: Option[Stream[A]] =
        everythingOpt map (_.toStream)
    }
  }

  def ∩ (other: Subgroup[A]): Subgroup[A] = this & other
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

  def apply[A](condition: A => Boolean)(implicit group: Group[A] with Cardinality[A]): Subgroup[A] = {
    new Subgroup[A] {
      override val set = Subset(condition)(this)
      def op(x: A, y: A) = group.op(x, y)
      lazy val everythingOpt = group.everythingOpt map (_ filter condition)
      lazy val generatorOpt = group.generatorOpt map (_ filter condition)
      def inverse(a: A) = group.inverse(a)
      def id = group.id
    }
  }
}

object SubgroupSyntax {

}
