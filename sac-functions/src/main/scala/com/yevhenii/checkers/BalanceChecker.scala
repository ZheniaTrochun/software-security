package com.yevhenii.checkers

import com.yevhenii.functions.BooleanFunction

object BalanceChecker {
  def check(f: BooleanFunction): Boolean = {
    (f.states.count(b => b).toDouble / f.states.size) == 0.5
  }
}
