package testing

import lfsr.LFSR
import lfsr.LFSR.Bit

object TestingUtils {

  def generateSequence(lfsr: LFSR, size: Int): Seq[Bit] = {
    lfsr.stream().take(size)
  }
}
