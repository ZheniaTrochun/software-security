package testing

import lfsr.LFSR


object SpreadTesting {
  def test(lfsr: LFSR, size: Int): Int = {
    val numbers = (for (_ <- 1 to size)
      yield lfsr.next()).groupBy(bit => bit).map(_._2.size).toList

    Math.abs(numbers(0) - numbers(1))
  }
}
