package com.yevhenii.reduction

object Parts {

  val wordLength = 16
  val procLength = 8
  val partNum = wordLength / procLength

  def splitToParts(num: Int): List[Int] = {
    val mask = (1 << procLength) - 1

    (for (i <- 0 until partNum)
      yield (num >> (procLength * i)) & mask)
      .toList
  }

  def collect(parts: List[Int]): Int = {
    val reversed = parts.reverse.toVector

    (for (i <- parts.indices)
      yield parts(i) << (procLength * i))
      .sum
  }

  def multiplyPart(part: Int, number: List[Int]): Int = {
    (for (i <- number.indices)
      yield (part * number(i)) << (procLength * i))
      .sum
  }
}