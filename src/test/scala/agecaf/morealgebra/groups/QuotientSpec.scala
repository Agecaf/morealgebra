package agecaf.morealgebra.groups

import agecaf.morealgebra.sets._
import org.scalatest._

class QuotientSpec extends FlatSpec with Matchers with NonImplicitAssertions {
  behavior of "ROUGH WORKING"

  it should "showcase quotients" in {
    val G = Int_+

    val Q = {
      implicit val group = G
      Quotient[Int]{ (a, b) => (a - b) % 20 == 0 }
    }

    val K = Quotient[Int]{ (a, b) => (a - b) % 5 == 0 }(G)

    {
      implicit val group = Q

      assert( (45 |+| 106) === 11 )
    }
  }

  it should "showcase reducible quotients" in {
    val G = Double_*

    val Q = {
      implicit val group = G
      ReducibleQuotient[Double]{ x => if (x > 0) x else -x }
    }

    {
      implicit val group = Q

      -1.0.reduce shouldBe 1.0
      (2.0 |+| -2.0) === 4.0 shouldBe true
    }
  }
}
