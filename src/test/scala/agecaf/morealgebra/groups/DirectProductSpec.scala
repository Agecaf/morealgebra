package agecaf.morealgebra.groups

import org.scalatest._
import spire.algebra.Group

class DirectProductSpec extends FlatSpec with Matchers {

  behavior of "Direct Product"

  it should "make direct products" in {
    {
      implicit val G = DirectProduct(Int_+, Double_*)

      (2, 2.0) |+| (3, 3.0) shouldBe (5, 6.0)
      G.id shouldBe (0, 1.0)
      (1, 2.0).inverse shouldBe (-1, 0.5)
    }

    {
      implicit val G = Double_+ directProduct Double_+

      (1.0, 2.0) |+| (3.0, 4.0) shouldBe (4.0, 6.0)
    }

    {
      implicit val G: Group[((Int, Int), Int)] = Cyclic(2) × Cyclic(3) × Cyclic(4)

      // Hmmm... This notation works, but is misleading!
      // Though it'd be a nice syntax for permutations...
      (1 -> 2 -> 3) |+| (1 -> 1 -> 1) shouldBe G.id
    }
  }
}
