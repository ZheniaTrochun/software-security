package com.yevhenii

import com.yevhenii.exponent.ModularExponent

object Test {

  def main(args: Array[String]): Unit = {
    println(ModularExponent.exponent(13, 2, 103))
    println("Ferma's theorem A^(m - 1) mod m = 1; 13^102 mod 103 = " + ModularExponent.exponent(13, 102, 103))
  }
}
