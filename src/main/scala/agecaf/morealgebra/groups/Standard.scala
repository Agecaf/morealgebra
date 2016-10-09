package agecaf.morealgebra.groups

import agecaf.morealgebra.sets._
import spire.implicits._
import spire.algebra._

/** Collection of Standard groups. */
trait Standard {

  val Int_+ = new AbGroup[Int] with Countable[Int] {
    def id = 0
    def op(x: Int, y: Int) = x + y
    def inverse(a: Int) = -a
    lazy val generator = Stream.iterate(0)(x => if (x >= 0) -(x + 1) else -x)
  }

  val Double_* = new AbGroup[Double] with Uncountable[Double] {
    def inverse(a: Double) = 1.0 / a
    def id = 1.0
    def op(x: Double, y: Double) = x * y
  }

  val Double_+ = new AbGroup[Double] with Uncountable[Double] {
    def inverse(a: Double) = -a
    def id = 0.0
    def op(x: Double, y: Double) = x + y
  }

  val PlusMinusOne = new AbGroup[Int] with Finite[Int] {
    def inverse(a: Int): Int = -a
    def id: Int = 1
    def op(x: Int, y: Int): Int = x * y
    val everything = Set(-1, 1)
  }
}
