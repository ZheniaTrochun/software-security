import lfsr.LFSR.Bit
import lfsr.LinearLFSR
import testing.{DifferentialTest, NonLinearTest, RangTest, FrequencyTest}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main extends NonLinearTest with FrequencyTest with DifferentialTest with RangTest {
  val balanced64Polynom: List[Bit] = createBalancedPolynomial()
  val testSequnceSize = 20000
  val initialState = createInitialState()
  val lfsr = new LinearLFSR(balanced64Polynom, initialState)

  def main(args: Array[String]): Unit = {
    println(s"Nonlinear complexity: ${lfsr.nonLinearComplexity(testSequnceSize)}")
    println(s"Frequency test: ${lfsr.frequency(testSequnceSize)}")
    println(s"Differential test: ${lfsr.differential(testSequnceSize)}")

    println("Rang testing:")
    lfsr.randComplexity(testSequnceSize)
      .zipWithIndex
      .foreach(tuple => println(s"${tuple._2} -> ${tuple._1}"))
  }

  def createInitialState(): ArrayBuffer[Bit] = {
    val r = new Random()
    (for (_ <- 1 to 64) yield r.nextInt(2).toByte).to[ArrayBuffer]
  }

  def createBalancedPolynomial(): List[Bit] = {
    1.toByte ::
      1.toByte ::
      (for (_ <- 1 to 58) yield 0.toByte).toList ++
        List(1, 1, 0, 1).map(_.toByte)
  }
}
