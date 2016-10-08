/**Some Algebraic Structures on top of Spire.algebra.
  *
  * =Use=
  *
  * `morealgebra` uses [[https://github.com/non/spire Spire]] for its algebraic structures.
  * However, `morealgebra` focuses on providing a lot of groups, rings and fields, as well
  * as ways to combine and extend them.
  *
  * '''Therefore, all the implicits provided in `morealgebra` are syntax implicits.'''
  *
  * The preferred style of doing group member operations in `morealgebra` is
  *  - Starting a block,
  *  - Declaring eximplicitly(*) which structure is used in the block,
  *  - And finally using the structure operations.
  *
  *  (*) '''Eximplicitly''': You, the user, have to write what implicit you're using.
  *
  * {{{
  *   import agecaf.morealgebra.groups._
  *
  *   // Well, we have two different Groups on Double, what do we do?
  *   val N: Group[Double] = Double_+
  *   val H: Group[Double] = Double_*
  *
  *   // Start a block
  *   {
  *     // Eximplicitly declare which structure we're using.
  *     implicit val G = N
  *
  *     // Use the operations
  *     1.0 |+| 2.0  // 1 + 2 = 3.0
  *   }
  *
  *   {
  *     implicit val G = H
  *
  *     1.0 |+| 2.0  // 1 * 2 = 2.0
  *   }
  *
  *   // This becomes more apparent when using more complex structures.
  *   // For example, both direct and semidirect products act on tuple2s.
  *   // This also exemplifies how you can operate on structures themselves,
  *   // to make larger structures
  *
  *   val D: Group[(Double, Double)] =
  *     DirectProduct(N, H)
  *
  *   val S: Group[(Double, Double)] =
  *     SemidirectProduct(N, H, {(x: Double) => (y: Double) => x*y}
  *
  *   {
  *     implicit val G = D
  *
  *     (1.0, 2.0) |+| (3.0, 4.0) // (1 + 3, 2 * 4) = (4.0, 8.0)
  *   }
  *
  *   {
  *     implicit val G = S
  *
  *     (1.0, 2.0) |+| (3.0, 4.0) // (1 + 2 * 3, 2 * 4) = (7.0, 8.0)
  *   }
  *
  * }}}
  *
  *
  * =Index=
  *
  * ==Groups==
  *
  * See [[agecaf.morealgebra.groups]] for more details.
  *
  *  - Some Standard Groups for common types such as `Int`, `Double`.
  *
  *  - Cyclic
  *
  *  - Direct and Semidirect Products
  */