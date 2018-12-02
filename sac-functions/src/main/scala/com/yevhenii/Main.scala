package com.yevhenii

import com.yevhenii.checkers.{AvalanchCriteriaChecker, BalanceChecker}
import com.yevhenii.generators.BooleanFunctionGenerator

object Main extends App {

//  val generated4All = BooleanFunctionGenerator.generateAllPossible(4)
//  val generated4Sac = BooleanFunctionGenerator.generateSacOnly(4)
//  val balanced4Sac = generated4Sac.filter(BalanceChecker.check)
//
//  println("==================== 4 variables ====================")
//  println(s"There are: ${generated4All.size} functions")
//  println(s"There are: ${generated4Sac.size} SAC functions")
//  println(s"There are: ${balanced4Sac.size} balance SAC functions")

  println("ortho sac and reversed ortho sac functions:")

  val start = System.currentTimeMillis()
  val system = BooleanFunctionGenerator.findAllOrthoSacSystems(4, 1).head
  val end = System.currentTimeMillis()

  println(s"${system.toString}")
  println(s"Reversed:\n${system.reverse()}")
  println(s"Is reversed SAC:\n${AvalanchCriteriaChecker.reverseSystemCheck(system)}")
  println(s"Time: ${end - start}ms")
}
