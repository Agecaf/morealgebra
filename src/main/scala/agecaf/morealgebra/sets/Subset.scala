package agecaf.morealgebra.sets

/** Subsets of a certain type.
  *
  * Extrapolates the idea of sets in scala to include "sets given a property".
  *
  * It includes useful set operations, as well as pattern matching to test
  * containment.
  *
  * @see The companion object contains the constructors (this class is abstract).
  *
  * =Use=
  *
  *  {{{
  *  {
  *    // A cardinality must be implicitly available
  *    val card: Cardinality[Double] = new Uncountable[Double]
  *
  *    // Declaring Subsets.
  *    val A: Subset[Double] = Subset {x: Double => x > 2}
  *    val B = Subset {Math.abs((_: Double)) <= 5}
  *    val C = Subset(Set(1.0, 2.0, 3.0))
  *
  *    // Testing for containment
  *    A contains 3.0 // true
  *    A contains 1.0 // false
  *
  *    // Subset operations
  *
  *    // Intersection
  *    val X1: Subset[Double] = A & B
  *    val X2 = A ∩ B
  *
  *    // Union
  *    val X3 = A | B
  *    val X4 = A ++ B
  *    val X5 = A ∪ B
  *
  *    // Set minus
  *    val X6 = A \ B
  *    val X7 = A -- B
  *
  *    // Complement
  *    val X8 = ~A
  *
  *    // Pattern Matching
  *    // Val definitions
  *    val A(x) = 3.0 // x = 3.0
  *    val A(y) = 1.0 // Match error, as 1.0 < 2.0, so 1.0 does not belong to A.
  *
  *    // Match cases
  *    def f(x: Double) = {
  *      case A(x) => "x is greater than 2."
  *      case _ => "x is lesser than 2."
  *    }
  *
  *    // For expressions
  *    for (A(x) <- List(0.5, 1.5, 2.5, 3.5)) {
  *      println(s"$x is greater than 2")
  *    }
  *
  *  }
  *  }}}
  *
  *
  * @tparam A The type of the subset. It should have a Cardinality
  *           typeclass present implicitly.
  */
abstract class Subset[A : Cardinality] {

  /** Subset Intersection.
    *
    * @param other the subset to intersect with. Subset[A]
    * @return the intersection of both subsets. Subset[A]
    */
  def & (other: Subset[A]): Subset[A]

  /** Subset Union.
    *
    * @param other the subset to unite with. Subset[A]
    * @return the union of both subsets. Subset[A]
    */
  def | (other: Subset[A]): Subset[A]

  /** Subset Difference (Set minus).
    *
    * @param other the subset whose elements to remove. Subset[A]
    * @return the set difference. Subset[A]
    */
  def \ (other: Subset[A]): Subset[A]

  /** Subset Complement.
    *
    * @return this subset's complement. Uses Cardinality[A] for finite ambient spaces.
    */
  def unary_~ : Subset[A]

  /** Whether an element forms part of the subset.
    *
    * @param x the element to test.
    * @return whether it is contained in this subset.
    */
  def contains(x: A): Boolean

  // Default Methods.
  /** Subset Union.
    *
    * @param other the subset to unite with. Subset[A]
    * @return the union of both subsets. Subset[A]
    */
  def ++ (other: Subset[A]): Subset[A] =
    this | other

  /** Subset Difference (Set minus).
    *
    * @param other the subset whose elements to remove. Subset[A]
    * @return the set difference. Subset[A]
    */
  def -- (other: Subset[A]): Subset[A] =
    this \ other

  /** Subset Intersection.
    *
    * @param other the subset to intersect with. Subset[A]
    * @return the intersection of both subsets. Subset[A]
    */
  def ∩ (other: Subset[A]): Subset[A] =
    this & other

  /** Subset Union.
    *
    * @param other the subset to unite with. Subset[A]
    * @return the union of both subsets. Subset[A]
    */
  def ∪ (other: Subset[A]): Subset[A] =
    this | other

  /** Matches if the element provided belongs to this subset.
    * ==Examples==
    *
    * {{{
    *    // Val definitions
    *    val A(x) = 3.0 // x = 3.0
    *    val A(y) = 1.0 // Match error, as 1.0 < 2.0, so 1.0 does not belong to A.
    *
    *    // Match cases
    *    def f(x: Double) = {
    *      case A(x) => "x is greater than 2."
    *      case _ => "x is lesser than 2."
    *    }
    *
    *    // For expressions
    *    for (A(x) <- List(0.5, 1.5, 2.5, 3.5)) {
    *      println(s"$x is greater than 2")
    *    }
    * }}}
    *
    * @param x the element to test.
    * @return Some(x) if x belongs to this subset. None otherwise.
    */
  def unapply(x: A): Option[A] =
    Some(x) filter contains

}

/** Constructors for [[Subset]]
  *
  * Also has private implementations.
  */
object Subset {
  /** Constructs a Subset from a property.
    *
    * Defines an abstract subset on type `A` based on a property `A => Boolean`.
    * Essentially "everything filter property", but for "infinite" sets as well.
    *
    * @param prop the property which defines our new subset.
    * @tparam A the type of our new subset.
    * @return our new subset.
    */
  def apply[A: Cardinality](prop: A => Boolean): Subset[A] = PropertySet(prop)

  /** Constructs a Subset from a scala Set.
    *
    * @param set the scala Set which defines our new subset.
    * @tparam A the type of our new subset.
    * @return our new subset.
    */
  def apply[A: Cardinality](set: Set[A]): Subset[A] = FiniteSet(set)


  /*
  Subset implementation for finite subsets.
   */
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

  /*
  * Implementation for a Subset based on properties.
  * */
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
