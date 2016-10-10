package agecaf.morealgebra.sets

import org.scalatest._
import agecaf.morealgebra.groups._


class SubsetSpec extends FlatSpec with Matchers {
  behavior of "Subsets"

  it should "declare Subsets." in {
    /*
    * To begin with, Subsets need an implicit Cardinality. This is to
    * optimize for finite sets (as then there is a "computable everything")
    * for set negation.
    *
    * You use the Subset factory with either a Set, or a Property.
    * This takes care to choose the adequate (private) implementation of Subset.
    *
    * The most basic subset method is `contains`.
    * */

    {
      implicit val Omega: Cardinality[Double] = Double_+

      // Declaring Subsets.
      val A1 = Subset {x: Double => x > 2}
      val A2 = Subset {x: Double => Math.abs(x) <= 5}
      val A3 = Subset(Set(1.0, 2.0, 3.0))

      assert(A1 contains 10.0)
      assert(!(A1 contains 0.0))

      assert(A2 contains 0.0)
      assert(!(A2 contains 10.0))

      assert(A3 contains 1.0)
      assert(!(A3 contains 1.5))
    }
  }

  it should "Allow pattern matching to test inclusion" in {
    /*
    * Each Subset instance allows for pattern matching to test elements.
    *
    * It can be used with
    *
    * - val definitions to ensure a value is in the subset.
    *   It throws match errors otherwise. An use case is with defining members
    *   of Subgroups; You want to make sure they indeed belong to the set, and
    *   want to be informed at compile time if otherwise.
    *
    * - match cases to match whether an element belongs to a subset.
    *
    * - for expressions can be used similar to other for expressions as a filter,
    *   or to do a thing if something is in the subset.
    *
    * */

    {
      implicit val Omega: Cardinality[Double] = Double_+

      val A = Subset {(_: Double) > 2}


      // Val definitions.
      val A(x) = 3.0
      x shouldBe 3.0

      assertThrows[MatchError] {
        val A(y) = 1.0
      }

      // Match expressions.
      def f: Double => Double = {
        case A(y) => y + 1.0
        case _ => 0.0
      }

      f(3.0) shouldBe 4.0
      f(1.5) shouldBe 0.0


      // For expressions
      var sum = 0.0
      for (A(x) <- Some(3.0)) {sum += x}
      sum shouldBe 3.0

      for (A(x) <- Some(1.0)) {sum = 0.0}
      sum shouldBe 3.0

      sum = 0.0
      val ls = List(0.5, 1.5, 2.5, 3.5)

      for (A(x) <- ls) {sum += x}
      sum shouldBe 6.0

      val twice = for (A(x) <- ls) yield x * 2
      twice shouldBe List(5.0, 7.0)
    }
  }

  it should "allow for set operations." in {
    /*
    * Subsets allow for common subset operations. When applicable,
    * there's both the "scala set" notation and the "math notation"
    * available.
    * */
    {
      implicit val Omega: Cardinality[Int] = Int_+

      val A = Subset(Set(0, 1, 2, 3, 4, 5))
      val B = Subset {x: Int => x % 2 == 0}

      // Note this is a scala Set, not a morealgebra Subset
      // Will be used for tests. Subsets do not allow for filtering,
      // mapping and so on because they might not be finite.
      val C = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      // Intersection, 'in A and B'.
      C filter (A & B contains _) shouldBe Set(0, 2, 4)
      C filter (A ∩ B contains _) shouldBe Set(0, 2, 4)

      // Union, 'in A or B'.
      C filter (A | B contains _) shouldBe Set(0, 1, 2, 3, 4, 5, 6, 8, 10)
      C filter (A ++ B contains _) shouldBe Set(0, 1, 2, 3, 4, 5, 6, 8, 10)
      C filter (A ∪ B contains _) shouldBe Set(0, 1, 2, 3, 4, 5, 6, 8, 10)

      // "Set minus"
      C filter (A \ B contains _) shouldBe Set(1, 3, 5)
      C filter (A -- B contains _) shouldBe Set(1, 3, 5)

      // Complementary set
      C filter (~A contains _) shouldBe Set(6, 7, 8, 9, 10)

    }
  }
}
