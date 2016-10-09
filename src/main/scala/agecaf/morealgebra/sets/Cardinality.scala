package agecaf.morealgebra.sets

trait Cardinality[A] {
  val everythingOpt: Option[Set[A]]
  val generatorOpt: Option[Stream[A]]

  lazy val isFinite = everythingOpt.isDefined
  lazy val isCountable = generatorOpt.isDefined
}







