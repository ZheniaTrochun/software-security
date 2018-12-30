package com.yevhenii

import com.yevhenii.reduction.Montgomery

object Test {

  def main(args: Array[String]): Unit = {
    println(s"Ferma's theorem A^(m - 1) mod m = 1; 13^106 mod 107 = ${Montgomery.modExp(13, 106, 107)}")
    println(s"13^2 mod 103 = ${Montgomery.modExp(13, 2, 103)}; should be 66")
  }
}
