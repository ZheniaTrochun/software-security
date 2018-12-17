package com.yevhenii.exponent

import com.yevhenii.reduction.Montgomery

object ModularExponent {

  def exponent(a: Int, e: Int, m: Int): Int = {
    var result = 1

    val binaryA = a.toBinaryString.reverse
    val n = binaryA.length
    val correlation = (2 << (n - 1)) % m

    for (ei <- e.toBinaryString) {
      result = Math.pow(result, 2).toInt % m

      if (ei == '1') {
        result = Montgomery.reduction(result, a, m)
      }
    }

    result
  }
}
