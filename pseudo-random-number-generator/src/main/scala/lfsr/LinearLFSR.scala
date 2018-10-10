package lfsr

import lfsr.LFSR._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class LinearLFSR(override val polynomial: List[Bit], override val register: ArrayBuffer[Bit]) extends LFSR {

  override def next(): Bit = {
    val nextValue = register.head
    updateRegister(nextValue)
    nextValue
  }

  override def stream(): Stream[Bit] = {
    next() #:: stream()
  }

  private def updateRegister(nextBit: Bit): Unit = {
    @tailrec
    def loop(curr: Int): Unit = {
      if (curr == polynomial.size - 1) {
        register(curr) = nextBit
      } else {
        register(curr) =
          if (polynomial(curr + 1) == 1)
            register(curr + 1) xor nextBit
          else
            register(curr + 1)
        loop(curr + 1)
      }
    }

    loop(0)
  }
}
