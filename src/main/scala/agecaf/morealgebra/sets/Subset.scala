package agecaf.morealgebra.sets

abstract class Subset[A : Cardinality] {
  def & (other: Subset[A]): Subset[A]
  def | (other: Subset[A]): Subset[A]
  def \ (other: Subset[A]): Subset[A]
  def unary_~ : Subset[A]
  def contains(x: A): Boolean

  // Default Methods.
  def &~ (other: Subset[A]): Subset[A] =
    (this | other) \ (this & other)

  def ++ (other: Subset[A]): Subset[A] =
    this & other

  def -- (other: Subset[A]): Subset[A] =
    this \ other

  def unapply(x: A): Option[A] =
    Some(x) filter contains
}

object Subset {
  def apply[A: Cardinality](prop: A => Boolean): Subset[A] = PropertySet(prop)
  def apply[A: Cardinality](set: Set[A]): Subset[A] = FiniteSet(set)


  private case class FiniteSet[A : Cardinality](members: Set[A]) extends Subset[A] {
    def & (other: Subset[A]): Subset[A] = other match {
      case FiniteSet(s) => FiniteSet(members & s)
      case PropertySet(c) => FiniteSet(members filter c)
    }

    def | (other: Subset[A]): Subset[A] = other match {
      case FiniteSet(s) => FiniteSet(members | s)
      case PropertySet(c) =>
        implicitly[Cardinality[A]].everythingOpt match {
          case Some(omega) => FiniteSet(members | (omega filter c))
          case None => PropertySet({x => (members contains x) || c(x)})
        }
    }

    def \ (other: Subset[A]): Subset[A] = other match {
      case FiniteSet(s) => FiniteSet(members -- s)
      case PropertySet(c) => FiniteSet(members filter (!c(_)))
    }

    def unary_~ : Subset[A] = implicitly[Cardinality[A]].everythingOpt match {
      case Some(omega) => FiniteSet(omega -- members)
      case None => PropertySet({x => !(members contains x)})
    }

    def contains(x: A): Boolean = members contains x
  }

  private case class PropertySet[A : Cardinality](prop: A => Boolean) extends Subset[A] {
    def & (other: Subset[A]): Subset[A] = other match {
      case PropertySet(c) => PropertySet({x => prop(x) && c(x)})
      case s: FiniteSet[A] => s & this
    }

    def | (other: Subset[A]): Subset[A] = other match {
      case PropertySet(c) => PropertySet({x => prop(x) || c(x)})
      case s: FiniteSet[A] => s & this
    }

    def \ (other: Subset[A]): Subset[A] = other match {
      case PropertySet(c) => PropertySet({x => prop(x) && !c(x)})
      case s: FiniteSet[A] => s & this
    }

    def unary_~ : Subset[A] = PropertySet({x => !prop(x)})

    def contains(x: A): Boolean = prop(x)
  }
}
