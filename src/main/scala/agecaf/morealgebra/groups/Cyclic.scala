package agecaf.morealgebra.groups

import agecaf.morealgebra.sets.Finite
import spire.algebra._

/** Cyclic Group structure, on top of Int.
  *
  * @see [[https://en.wikipedia.org/wiki/Cyclic_group Cyclic Groups (Wikipedia)]]
  *
  * =Currently supported Structures=
  *
  *  - Abelian Group.
  *
  *  - Equivalence classes on Int.
  *
  *  - Finite on Int.
  *
  * =Use=
  *
  * {{{
  *   // Minimal imports
  *   import agecaf.morealgebra.groups.GroupSyntax._
  *   import agecaf.morealgebra.groups.Cyclic
  *
  *   // Alternatively
  *   // import agecaf.morealgebra.groups._
  *
  *   {
  *     // Declare your implicit group!
  *     implicit val C3 = Cyclic(3) // C3: Group[Int] with Eq[Int]
  *
  *     // Using Group[Int]
  *     1 |+| 2   // 3 === 0
  *     1 |-| 2   // -1 === 2
  *     1.inverse // 2
  *     C3.id     // 0
  *
  *     // Using Eq[Int]
  *     4 === 1 // true
  *     1 =!= 2 // true
  *
  *     // Due to operator precedence, the parentheses are unfortunately required.
  *     (2 |+| 2) === 1 // true
  *   }
  *
  *   {
  *     // Use blocks to use different implicit groups.
  *     implicit val C5 = Cyclic(5)
  *
  *     3 |+| 4 // 2
  *   }
  * }}}
  *
  * @note If you want to use the Cyclic Group's `Eq[Int]` structure (for modulo
  *       congruences), make sure to not import `spire.implicits._`, as this
  *       includes an overriding `Eq[Int]` structure.
  *
  */
class Cyclic(m: Int) extends AbGroup[Int] with Eq[Int] with Finite[Int] {
  assert(m > 0)
  def op(a: Int, b: Int): Int = {
    val x = (a + b) % m
    if (x < 0) x + m else x // Because mod is different for negative numbers.
  }
  def inverse(a: Int) = op(-a, 0)
  def id: Int = 0
  def eqv(a: Int, b: Int): Boolean = ((a - b) % m) == 0
  lazy val everything = Range(0, m - 1).toSet
}

/** Factory for Cyclic Groups.
  *
  * @see [[Cyclic]]
  */
object Cyclic {

  /** Factory method for Cyclic Groups
    *
    * @param order The order (size) of the cyclic group. Should be strictly greater than 0
    * @return The Cyclic Group, of type `AbGroup[Int] with Eq[Int]`
    */
  def apply(order: Int): Cyclic = {
    if (order <= 0) throw new IllegalArgumentException("Cyclic Group must have a strictly positive integer order.")
    new Cyclic(order)
  }
}
