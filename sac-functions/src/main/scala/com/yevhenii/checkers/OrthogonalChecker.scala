package com.yevhenii.checkers

import com.yevhenii.functions.BooleanFunctionSystem

object OrthogonalChecker {

  def checkSystem(system: BooleanFunctionSystem): Boolean = {
    var flag = true
    var i = 0

    while (flag && i < (system.size - 1)) {
      var j = i + 1
      while (flag && j < system.size) {
        if (system(i) == system(j))
          flag = false

        j +=1
      }
      i +=1
    }

    flag
  }
}
