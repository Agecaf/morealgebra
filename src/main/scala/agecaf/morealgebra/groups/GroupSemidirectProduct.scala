package agecaf.morealgebra.groups

import spire.algebra._

trait GroupSemidirectProduct {

  implicit class WithGroupSemidirectProduct[N](N: Group[N]) {
    def semidirectProduct[H] (H: Group[H], f: H => N => N): Group[(N, H)] =
      SemidirectProduct(N, H, f)

    def ⋊ [H] (H: Group[H], f: H => N => N): Group[(N, H)] = N semidirectProduct  (H, f)
  }

  def SemidirectProduct[N, H](N: Group[N], H: Group[H], f: H => N => N): Group[(N, H)] =
    new Group[(N, H)] {
      override def inverse(a: (N, H)): (N, H) =
        (f(H.inverse(a._2))(N.inverse(a._1)), H.inverse(a._2))


      override def id: (N, H) =
        (N.id, H.id)

      override def op(x: (N, H), y: (N, H)): (N, H) =
        (N.op(x._1, f(x._2)(y._1)), H.op(x._2, y._2))
    }
}
