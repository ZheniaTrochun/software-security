package com.yevhenii.exponent

import java.math.BigInteger

import com.yevhenii.reduction.Montgomery

object ModularExponent {

  def exponent(a: BigInt, e: BigInt, m: BigInt): BigInt = {
    var result: BigInt = 1

    val n = m.bitLength
    val correlation = (1 << n) % m


    for (i <- (0 until e.bitLength).reverse) {

      result = Montgomery.reduction(result, result, m)

      if (e.testBit(i)) {
        result = Montgomery.reduction(a, result, m)
      }
    }

    (result * correlation) % m
  }
}
