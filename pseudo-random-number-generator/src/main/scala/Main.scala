import lfsr.LFSR.Bit
import lfsr.LinearLFSR
import testing.{NonLinearTest, SpreadTesting}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main {
  val balanced64Polynom: List[Bit] = createBalancedPolynom()
  val size = 20000

  val initialState = createInitialState()
  val lfsr = new LinearLFSR(balanced64Polynom, initialState)

  def main(args: Array[String]): Unit = {
    println(NonLinearTest.calculateComplexity(lfsr, size))
    println(SpreadTesting.test(lfsr, size))
  }

  def createInitialState(): ArrayBuffer[Bit] = {
    val r = new Random()
    (for (_ <- 1 to 64) yield r.nextInt(2).toByte).to[ArrayBuffer]
  }

  def createBalancedPolynom(): List[Bit] = {
    1.toByte ::
      1.toByte ::
      (for (_ <- 1 to 58) yield 0.toByte).toList ++
        List(1, 1, 0, 1).map(_.toByte)
  }
}
