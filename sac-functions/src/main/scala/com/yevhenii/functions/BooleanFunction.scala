package com.yevhenii.functions

import com.yevhenii.functions.BooleanFunction._

case class BooleanFunction (states: List[Boolean]) {

  val n = log2(states.length).toInt

  val table = tables(n)

  def apply(n: Int): Boolean = {
    assert(n >= 0 && n < states.size)
    states(n)
  }

  val values: List[String] = {
    states.map {
      case false => "0"
      case true => "1"
    }
  }

  override def toString: String = {
    def convert(pair: (Boolean, Int)): String = {
      s"${formatBinaryString(pair._2.toBinaryString, n)}| ${if (pair._1) 1 else 0}"
    }

    "--- function ---\n" +
     table.zip(values)
       .map(row => s"${row._1}| ${row._2}")
       .mkString("\n")
  }
}

object BooleanFunction {
  def log2(x: Double): Double = Math.log10(x) / Math.log10(2)

  val tables = Map(
    2 -> (0 to 3).map(_.toBinaryString).map(row => formatBinaryString(row, 2)).toList,
    3 -> (0 to 7).map(_.toBinaryString).map(row => formatBinaryString(row, 3)).toList,
    4 -> (0 to 15).map(_.toBinaryString).map(row => formatBinaryString(row, 4)).toList
  )

  def formatBinaryString(str: String, n: Int): String = {
    val diff = n - str.length

    if (diff > 0) ("0" * diff) + str
    else str
  }
}
