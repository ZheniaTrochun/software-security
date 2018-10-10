package testing

import lfsr.LFSR
import lfsr.LFSR._
import testing.TestingUtils._

trait RangTest {

  implicit class randTestable(lfsr: LFSR) {
    def randComplexity(size: Int): List[Double] = {
      val seq = generateSequence(lfsr, size)
      val splittedMap =
        (for (i <- 0 until size - 3)
          yield seq.slice(i, i + 3))
          .groupBy(s => s)

      val res = splittedMap
        .map(tuple => tuple._1.mkString -> tuple._2.size)
        .toList
        .sortBy(_._1)
        .map(_._2.toDouble / size)

      assert(res.sum - 1.0 < 0.001)

      res
    }
  }
}
