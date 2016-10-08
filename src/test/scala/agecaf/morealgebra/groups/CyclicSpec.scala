package agecaf.morealgebra.groups

import org.scalatest._

class CyclicSpec extends FlatSpec with Matchers with NonImplicitAssertions {

  /*
    Note: "spire.implicits._" includes implicits for default structures,
          including a default Eq[Int] structure which clashes with Cyclic's.

          Prefer to use "spire.syntax.all._" instead when using Cyclic groups.
   */
  import spire.syntax.all._

  behavior of "Cyclic Group operation."

  it should "behave as expected for standard values." in {
    {
      implicit val G = Cyclic(10)

      12 |+| 23 shouldBe 5
    }

    {
      implicit val G = Cyclic(11)

      12 |+| 23 shouldBe 2
    }

    {
      implicit val G = Cyclic(9)

      12 |+| 23 shouldBe 8
    }
  }

  it should "behave as expected with negative numbers" in {
    implicit val G = Cyclic(100)

    // We must be careful to always return the value between 0 and 99,
    // As scala's % operator behaves differently for negative numbers.
    -101 |+| 450 shouldBe 49
    -401 |+| 150 shouldBe 49
    350 |+| -101 shouldBe 49
    150 |+| -301 shouldBe 49
    -101 |+| -102 shouldBe 97
  }

  "Cyclic Group Equivalence" should "respect modulo." in {

    {
      implicit val G = Cyclic(10)

      assert(1 === 11)
      assert(2 =!= 15)
    }

    {
      implicit val G = Cyclic(5)

      assert(-2 === 8)
      assert(-1 =!= 1)
    }

    {
      implicit val G = Cyclic(13)

      assert(-1 === -14)
      assert(-1 =!= -2)
    }
  }

  "Cyclic Group Identity" should "be the identity" in {
    {
      implicit val G = Cyclic(20)

      assert((G.id |+| 430) === 430)
      assert((430 |+| G.id) === 430)
    }

    {
      implicit val G = Cyclic(42)

      assert((G.id |+| 430) === 430)
      assert((430 |+| G.id) === 430)
    }
  }

  "Cyclic Group Inverse" should "be correct!" in {
    {
      implicit val G = Cyclic(15)

      // Simple inverse notation.
      assert(13.inverse === 2)
      assert((147 |+| 147.inverse) === G.id)
      assert((147.inverse |+| 147) === G.id)

      // We can also use the "minus" operation.
      assert((2 |-| 3) === -1)
    }
  }
}
