package testing

import lfsr.LFSR
import lfsr.LFSR.Bit

object TestingUtils {

  def generateSequence(lfsr: LFSR, size: Int): Seq[Bit] = {
    for (_ <- 1 to size) yield lfsr.next()
  }
}
