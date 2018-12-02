package com.yevhenii.checkers

import com.yevhenii.functions.{BooleanFunction, BooleanFunctionSystem}

object AvalanchCriteriaChecker {

  def reverseSystemCheck(sys: BooleanFunctionSystem): Boolean = {
    sys.reverse().functions.forall(check)
  }

  def check(func: BooleanFunction): Boolean = {
    (for (n <- 1 to BooleanFunction.log2(func.states.indices.size).toInt)
      yield differentialComplexityByX(n)(func))
        .forall(_ == 0.5)
  }

  def differentialComplexityByX(n: Int)(f: BooleanFunction): Double = {
    val table = f.table

    val grouped =
      table.zip(f.states)
        .map(row => row._1.patch(n - 1, "", 1) -> row._2)
        .groupBy(_._1)
        .map(pair => pair._1 -> pair._2.map(_._2))

    grouped.foreach(pair => assert(pair._2.size == 2))

    grouped.values
      .map(xor)
      .count(b => b)
      .toDouble / grouped.size
  }

  def xor(boolList: List[Boolean]): Boolean = {
    def xor2(b1: Boolean, b2: Boolean): Boolean = (b1, b2) match {
      case (true, true) => false
      case (false, false) => false
      case _ => true
    }

    boolList.reduce(xor2)
  }
}
