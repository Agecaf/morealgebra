package agecaf.morealgebra

package groups {
  /** Syntax imported from Spire.
    *
    * Includes [[https://github.com/non/spire#semigroup-monoid-and-group GroupSyntax]]
    * and [[https://github.com/non/spire#eq-order-and-partialorder EqSyntax]].
    *
    * @note This can be individually imported with `import agecaf.morealgebra.groups.GroupSyntax._`
    *
    */
  trait FromSpire
    extends spire.syntax.GroupSyntax
      with spire.syntax.EqSyntax

  /** Syntax for groups.
    *
    * @see [[FromSpire]]
    */
  object GroupSyntax extends FromSpire
}

/** Contains Constructors and Syntax for groups, and some Standard Groups.
  *
  * This package contains an ample set of constructor methods for groups,
  * as well as some basic groups. It additionally includes the group
  * syntax from Spire, so that further imports are not required.
  *
  * =Implicits=
  *
  *  - Spire's syntax implicit conversions acting on group members.
  *    This adds the |+| operator, for example.
  *
  *  - Spire's syntax implicit conversions for Eq.
  *    This adds the === operator, for example. Used for Cyclic groups for example.
  *
  *  - MoreAlgebra's group implicit conversions. These are for syntax sugar for
  *    DirectProducts for example. Note that these act on groups `Group[A]` rather than on
  *    group members `A`
  *
  * =Non Implicits=
  *
  *  - No instances of `Group[A]` are given implicitly. This is a design choice.
  *
  *  - No instances of `Eq[A]` are given implicitly. This is a design choice.
  *
  */
package object groups
  extends FromSpire
  with Standard
  with GroupDirectProduct
  with GroupSemidirectProduct
