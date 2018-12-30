package com.yevhenii.reduction

import java.math.BigInteger

object Montgomery {
  def reduction(a: BigInt, b: BigInt, m: BigInt): BigInt = {
    var result: BigInt = 0

    for (i <- 0 until a.bitLength) {
      if (a.testBit(i)) {
        result += b
      }
      result = (result + (result & 0x1) * m) >> 2
    }

    if (result >= m)
      result - m
    else
      result
  }

  def reduction2(a: BigInt, b: BigInt, m: BigInt): BigInt = {
    var result: BigInt = 0
    val n = a.bitLength
    val correlation = (1 << n) % m

    for (i <- 0 until n) {
      if (a.testBit(i)) {
        result += b
      }
      result = (result + (result & 0x1) * m) / 2
    }

    result = if (result >= m) result - m else result
    (result * correlation) % m
  }




  def modExp(a: BigInt, e: BigInt, m: BigInt): BigInt = {
    val r: BigInt = 1  << m.bitLength
    val nPrime = (- m).modInverse(r)

    def monPro(aBar: BigInt, bBar: BigInt) = {
      val t = aBar * bBar
      val reduced = (t * nPrime) & (r - 1)
      val u = ((reduced * m) + t) >> m.bitLength

      if (u >= m)
        u - m
      else
        u
    }

    def toNormalForm(x: BigInt): BigInt = {
      monPro(x, 1)
    }

    def toMontgomeryForm(a: BigInt): BigInt = {
      (a << m.bitLength) % m
    }

    val aBar = toMontgomeryForm(a)
    var xBar = toMontgomeryForm(1)


//    functional style
//    toNormalForm {
//      (0 until e.bitLength).reverse.foldLeft(xBar) { (x, bit) =>
//        if (e.testBit(bit))
//          monPro(aBar, monPro(x, x))
//        else
//          monPro(x, x)
//      }
//    }


//    imperative style
    for (i <- (0 until e.bitLength).reverse) {
      xBar = monPro(xBar, xBar)
      if (e.testBit(i)) {
        xBar = monPro(aBar, xBar)
      }
    }

    toNormalForm(xBar)
  }

}
