package testing

import lfsr.LFSR
import lfsr.LFSR._
import testing.TestingUtils._

trait DifferentialTest {

  implicit class differentialTestable(lfsr: LFSR) {
    def differential(size: Int): Double = {
      val seq = generateSequence(lfsr, size)
      val xored = (seq.init zip seq.tail)
        .map(tuple => tuple._1 xor tuple._2)
        .map(_.toDouble)

      xored.sum / xored.size
    }
  }
}
