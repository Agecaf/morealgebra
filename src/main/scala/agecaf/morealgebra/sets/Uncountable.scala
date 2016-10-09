package agecaf.morealgebra.sets

trait Uncountable[A] extends Cardinality[A] {
  val everythingOpt = None
  val generatorOpt = None
}
