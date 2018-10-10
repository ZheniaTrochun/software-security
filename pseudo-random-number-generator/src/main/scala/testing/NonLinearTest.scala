package testing

import lfsr.LFSR
import testing.TestingUtils._

import scala.annotation.tailrec

trait NonLinearTest {

  implicit class testable(lfsr: LFSR) {
    def nonLinearComplexity(size: Int): Int = {
      calculateComplexity(lfsr, size)
    }
  }

  private def calculateComplexity(lfsr: LFSR, size: Int): Int = {
    val seq = generateSequence(lfsr, size).mkString

    @tailrec
    def loop(n: Int): Int = {
      val isFound =
        0.until(size - n) forall { i =>
          applyCondition(seq.drop(i).take(n))
        }

      if (isFound)
        n
      else
        loop(n + 1)
    }

    def applyCondition(subseq: String): Boolean = {
      val first = seq.contains(subseq + "0")
      val second = seq.contains(subseq + "1")
      !(first && second)
    }

    loop(3)
  }
}

