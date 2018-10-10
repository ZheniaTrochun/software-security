package lfsr

import lfsr.LFSR.Bit

import scala.collection.mutable.ArrayBuffer

trait LFSR {

  val polynomial: List[Bit]
  val register: ArrayBuffer[Bit]

  def next(): Bit

  def stream(): Stream[Bit]
}

object LFSR {
  type Bit = Byte

  implicit class bit(byte: Byte) {
    def xor(other: Byte): Byte = {
      assert(((byte == 0) || (byte == 1)) && ((other == 0) || (other == 1)), "Both byte values should be 0 or 1")

      (byte, other) match {
        case (0, 0) => 0
        case (1, 1) => 0
        case (_, _) => 1
      }
    }
  }
}