package com.yevhenii.functions

case class BooleanFunctionSystem(functions: Vector[BooleanFunction]) {

  lazy val table: List[String] = functions.head.table

  val size: Int = functions.head.states.size

  def apply(n: Int): String = {
    assert(n >= 0 && n < functions.head.states.size)

    functions
      .map(f => f(n))
      .map(b => if (b) 1 else 0)
      .mkString
  }

  def reverse(): BooleanFunctionSystem = {
    val functions =
      table.zipWithIndex
        .map(pair => apply(pair._2) -> pair._1)
        .sortBy(_._1)
        .map(_._2)

    val reversed =
      for (i <- 0 until functions.head.length)
        yield functions.map(_.charAt(i)).map {
          case '0' => false
          case '1' => true
        }

    BooleanFunctionSystem(reversed.map(f => BooleanFunction(f)).toVector)
  }

  override def toString: String = {
    "--- functions ---\n" +
      table.zipWithIndex
        .map(pair => s"${pair._1}| ${apply(pair._2)}")
        .mkString("\n")
  }
}

