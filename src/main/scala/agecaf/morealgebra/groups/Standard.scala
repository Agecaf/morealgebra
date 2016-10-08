package agecaf.morealgebra.groups

import spire.implicits._
import spire.algebra._

/** Collection of Standard groups. */
trait Standard {

  val Int_+ : AbGroup[Int] = Ring[Int].additive

  val Double_* : AbGroup[Double] = Field[Double].multiplicative
  val Double_+ : AbGroup[Double] = Field[Double].additive

}
