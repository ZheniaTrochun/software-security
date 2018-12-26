package com.yevhenii

import com.yevhenii.exponent.ModularExponent
import com.yevhenii.reduction.Montgomery

object Test {

  def main(args: Array[String]): Unit = {
    println(ModularExponent.exponent(13, 2, 103))
//    println(Montgomery.exponentiation(71, 106, 107))
//    println(Montgomery.reduction(21, 59, 107))
    println("Ferma's theorem A^(m - 1) mod m = 1; 13^102 mod 103 = " + ModularExponent.exponent(71, 106, 107))
    println("Ferma's theorem A^(m - 1) mod m = 1; 13^102 mod 103 = " + Montgomery.modExp(13, 106, 107))
    println("13^2 mod 103 = " + Montgomery.modExp(13, 2, 103) + "; should be 66")

  }
}
