package agecaf.morealgebra.groups

import org.scalatest._
import agecaf.morealgebra.sets._


class SubgroupSpec extends FlatSpec with Matchers {
  behavior of "ROUGH WORKING"

  it should "showcase subgroups" in {
    val G = Int_+

    // Use implicits to define subgroups.
    val H = {
      implicit val group = G
      Subgroup{ (_: Int) % 3 == 0 }
    }

    // Or explicitly...
    val I = Subgroup{ (_: Int) % 4 == 0 }(G)

    // You can check for containment, like Subsets.
    H contains 18 shouldBe true
    I contains 18 shouldBe false

    // And you can operate with them, like groups.
    {
      implicit val group = H

      val H(a) = 3
      val H(b) = 6

      a |+| b shouldBe 9
      H contains (a |-| b) shouldBe true
      H.id shouldBe 0
      a.inverse shouldBe -3
    }

    // You can also use the unapply functionality, similar to Subsets.
    val H(x) = 6
    x shouldBe 6

    assertThrows[MatchError]{
      val I(y) = 6
    }

    // Subgroup unapply can also be used in pattern matching and for comprehensions.

    // You can also intersect Subgroups.
    val J = H & I
    J contains 3 shouldBe false
    J contains 4 shouldBe false
    J contains 12 shouldBe true

  }
}
