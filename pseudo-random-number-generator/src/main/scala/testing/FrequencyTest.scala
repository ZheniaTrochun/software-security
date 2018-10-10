package testing

import lfsr.LFSR
import testing.TestingUtils._

trait FrequencyTest {

  implicit class frequentlyTestable(lfsr: LFSR) {
    def frequency(size: Int): Double = {
      val seq = generateSequence(lfsr, size)
      seq.map(_.toDouble).sum / seq.size
    }
  }
}
