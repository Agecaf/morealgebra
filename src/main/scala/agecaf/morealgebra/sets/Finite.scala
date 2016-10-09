package agecaf.morealgebra.sets

trait Finite[A] extends Countable[A] {
  val everything: Set[A]
  lazy val generator = everything.toStream
  override lazy val everythingOpt = Some(everything)
}
