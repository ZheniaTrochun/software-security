package com.yevhenii.reduction

object Montgomery {
  def reduction(a: Int, b: Int, m: Int): Int = {
    var result: Int = 0
    val binaryA = a.toBinaryString.reverse
    val n = binaryA.length
    val correlation = (2 << (n - 1)) % m

    for (i <- 0 until n) {
      if (binaryA(i) == '1') {
        result += b
      }
      result = (result + (result & 0x1) * m) / 2
    }

    (result * correlation) % m
  }
}
