package agecaf.morealgebra.groups

import org.scalatest.{FlatSpec, Matchers}
import spire.algebra.Group

class SemidirectProductSpec extends FlatSpec with Matchers {

  behavior of "Semidirect Products"

  it should "make Semidirect Products" in {
    {
      implicit val G = SemidirectProduct(Double_+, Double_*, {(h: Double) => (n: Double) => h * n})

      (1.0, 2.0) |+| (3.0, 4.0) shouldBe (7.0, 8.0) // (1 + 2 * 3, 2 * 4)
      G.id shouldBe (0.0, 1.0)
      (1.0, 2.0).inverse shouldBe (-0.5, 0.5) // (1 + 2 * (-0.5), 2 * 0.5) == (0, 1)
    }

    {
      implicit val G = Double_+ semidirectProduct (PlusMinusOne, {(h: Int) => (n: Double) => h * n})

      (1.0, -1) |+| (2.0, 1) shouldBe (-1.0, -1) // (1 + (-1) * 2, (-1) * (1))
    }

    {
      // Dihedral Group. (rotation, symmetry)
      implicit val D8 = Cyclic(4) ⋊ (PlusMinusOne, {(h: Int) => (n: Int) => (h*n+4)%4 })

      (2, -1) |+| (1, 1) shouldBe (1, -1)
    }

    {
      // There'll be proper vector groups. Soon. Maybe. Also rotation groups.
      // In the meantime, you can already do cool stuff with products!
      implicit val Placement =
        Double_+
          .directProduct(Double_+)
          .semidirectProduct(Double_+, {
            (h: Double) => (n: (Double, Double)) =>
              val s = Math.sin(h)
              val c = Math.cos(h)
              (c * n._1 - s * n._2, s * n._1 + c * n._2)
          })

      val translation = (1.0, 0.0) -> 0.0
      val rotation = (0.0, 0.0) -> Math.PI

      // id :   >
      // r  :   <
      // t  :   +  >
      // tr :   +  <
      // trt:   <
      val ans = translation |+| rotation |+| translation

      // ans shouldBe rotation // Almost, but tolerance with sin / cos.

    }
  }
}
