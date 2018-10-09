package testing

import lfsr.LFSR
import lfsr.LFSR._

import scala.annotation.tailrec

class NonLinearTest(lfsr: LFSR, size: Int) {
  import NonLinearTest._

  lazy val complexity: Int = calculateComplexity(lfsr, size)
}

object NonLinearTest {

  def calculateComplexity(lfsr: LFSR, size: Int): Int = {
    val seq = (for (_ <- 1 to size) yield lfsr.next()).mkString

    @tailrec
    def loop(n: Int): Int = {
      val isFound =
        0.until(size - n) forall { i =>
          val subseq = seq.drop(i).take(n)
          val first = seq.contains(subseq + "0")
          val second = seq.contains(subseq + "1")
          !(first && second)
        }

      if (isFound)
        n
      else
        loop(n + 1)
    }

    loop(3)
  }

  def calculateComplexityFull(lfsr: LFSR, size: Int): Int = {
    val seq = generateSequence(lfsr, size).mkString

    @tailrec
    def loop(n: Int, prev: List[String]): Int = {
      val table = extendTable(prev)
      val temp = table.par.forall(list => applyCondition(seq, list))

      if (n > seq.size)
        throw new IllegalArgumentException("incorrect behaviour")

      if (temp) {
        n
      } else {
        loop(n + 1, table)
      }
    }

    loop(1, "0" :: "1" :: Nil)
  }

  def generateSequence(lfsr: LFSR, size: Int): List[Bit] = {
    (for (_ <- 1 to size) yield lfsr.next()).toList
  }

  def extendTable(prev: List[String]): List[String] = {
    prev.map(row => s"0$row") ++ prev.map(row => s"1$row")
  }

  def extendTable(prev: Vector[String]): Vector[String] = {
    prev.map(row => s"0$row") ++ prev.map(row => s"1$row")
  }

  def applyCondition(seq: String, subseq: String): Boolean = {
    val first = seq.contains(s"${subseq}0")
    val second = seq.contains(s"${subseq}1")

    !(first && second)
  }
}