package agecaf.morealgebra.sets

trait Countable[A] extends Cardinality[A] {
  val generator: Stream[A]
  override lazy val everythingOpt: Option[Set[A]] = None
  override lazy val generatorOpt = Some(generator)
}
