package com.yevhenii.generators

import com.yevhenii.checkers.{AvalanchCriteriaChecker, BalanceChecker, OrthogonalChecker}
import com.yevhenii.functions.{BooleanFunction, BooleanFunctionSystem}


object BooleanFunctionGenerator {

  def generateAllPossible(n: Int): List[BooleanFunction] = {
    def loop(depth: Int, finish: Int, lists: List[List[Boolean]]): List[List[Boolean]] = {
      if (depth == finish) {
        lists
      } else loop(
        depth + 1,
        finish,
        lists.map(false :: _) ++ lists.map(true :: _)
      )
    }

    loop(0, Math.pow(2, n).toInt, List(Nil))
      .map(states => BooleanFunction(states))
  }

  def generateSacOnly(n: Int): List[BooleanFunction] = {
    generateAllPossible(n).filter(AvalanchCriteriaChecker.check)
  }

//  todo why this works ????????????????????????????
//  todo without "+ 50" doesn't work
  def generateOrthogonal(functions: Vector[BooleanFunction]): Option[BooleanFunctionSystem] = {
    for {
      i <- 0 until (functions.size - 3)
      j <- (i + 50) until functions.size - 2
      k <- (j + 50) until functions.size - 1
      m <- (k + 50) until functions.size
    } {
      val system = BooleanFunctionSystem(Vector(functions(i), functions(j), functions(k), functions(m)))
      if (OrthogonalChecker.checkSystem(system))
        return Some(system)
    }

    None
  }

  def findAllOrthoSacSystems(n: Int, numberToFind: Int): List[BooleanFunctionSystem] = {
    val data = new GeneratorData(n)

    data.sacFunctions.toStream.flatMap(f =>
      matchWithTable(f, data)
        .filterNot(_.isEmpty)
        .map(list => BooleanFunctionSystem(list.toVector))
        .filter(OrthogonalChecker.checkSystem)
        .filter(AvalanchCriteriaChecker.reverseSystemCheck)
    ).take(numberToFind).toList
  }

  def matchWithTable(func: BooleanFunction, data: GeneratorData): Stream[List[BooleanFunction]] = {

    type Functions = Vector[String]

    def loop(curr: Stream[List[BooleanFunction]], zeros: List[Functions], ones: List[Functions], values: List[Char]): Stream[List[BooleanFunction]] = {
      if (zeros.isEmpty || ones.isEmpty) {
        curr
      } else {
        val currZeros = zeros.head
        val currOnes = ones.head

        val updated = toBooleanFunctions(
          zipWithCharIndex(values)
            .map(pair =>
              if (pair._1 == '0') currZeros(pair._2)
              else currOnes(pair._2)
            )
        )

        loop(updated #:: curr, zeros.tail, ones.tail, values)
      }
    }

    loop(Stream(Nil), data.zeroPermutations, data.onePermutations, func.values.map(_.head))
  }

  def toBooleanFunctions(strings: List[String]): List[BooleanFunction] = {
    (for (i <- 0 until strings.head.length)
      yield BooleanFunction(
        strings.map(_.charAt(i)).map {
          case '0' => false
          case '1' => true
        }
      ))
      .toList
  }

  def zipWithCharIndex(func: List[Char]): List[(Char, Int)] = {
    var ones = 0
    var zeros = 0

    func.map {
      case c @ '0' =>
        zeros += 1
        c -> (zeros - 1)

      case c @ '1' =>
        ones += 1
        c -> (ones - 1)
    }
  }
}

class GeneratorData(n: Int) {
  val table = BooleanFunction.tables(n)

  val sacFunctions: List[BooleanFunction] = BooleanFunctionGenerator.generateSacOnly(n).filter(BalanceChecker.check)

//  permutations
  val zeroPermutations: List[Vector[String]] = util.Random.shuffle(GeneratorData.permutationsVec(table.filter(_.head == '0')))
  val onePermutations: List[Vector[String]] = util.Random.shuffle(GeneratorData.permutationsVec(table.filter(_.head == '1')))
}

object GeneratorData {
  def permutations[A](list: List[A]): List[List[A]] = {
    def loop(result: List[List[A]], curr: List[A]): List[List[A]] = {
      if (curr.length == 2)
        result.map(curr.head :: curr.last :: _) ++ result.map(curr.last :: curr.head :: _)
      else
        curr.flatMap(item => loop(result.map(item :: _), curr.filterNot(_ == item)))
    }

    loop(List(Nil), list)
  }

  def permutationsVec[A](list: List[A]): List[Vector[A]] = {
    def loop(result: List[Vector[A]], curr: List[A]): List[Vector[A]] = {
      if (curr.length == 2)
        result.map(curr.head +: curr.last +: _) ++ result.map(curr.last +: curr.head +: _)
      else
        curr.flatMap(item => loop(result.map(item +: _), curr.filterNot(_ == item)))
    }

    loop(List(Vector()), list)
  }
}
