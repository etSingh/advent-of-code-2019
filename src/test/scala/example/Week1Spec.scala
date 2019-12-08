package example

import example.Day4.{compute, matchesAtLeastCriteria, matchesAtMostCriteria}
import org.scalatest._

import scala.collection.immutable.ArraySeq

class Week1Spec extends FlatSpec with Matchers {

  "Should give correct answers for" should "day 1" in {
    Day1.part1().map(
      ans => ans shouldEqual 3348430
    )

    Day1.part2().map(
      ans => ans shouldEqual 5019767
    )
  }

  "Should give correct answers for" should "day 2" in {
    IntCode.part1().map(
      ans => ans shouldEqual 3058646
    )

    IntCode.part2().map(
      ans => ans.head shouldEqual 8976
    )
  }

  "Should give correct answers for" should "day 4" in {
    compute(136818, 685979, matchesAtMostCriteria) shouldEqual 1291
    compute(136818, 685979, matchesAtLeastCriteria) shouldEqual 1919
  }

  "IntCode computer for day 5 part 2" should "work" in {
    val eqToEight = ArraySeq.from(List(3,9,8,9,10,9,4,9,99,-1,8))
    val eqToEightImmediateMode = ArraySeq.from(List(3,3,1108,-1,8,3,4,3,99))

    Day5.getOutput(eqToEight, 8) shouldEqual 1
    Day5.getOutput(eqToEight, 10) shouldEqual 0
    Day5.getOutput(eqToEightImmediateMode, 8) shouldEqual 1
    Day5.getOutput(eqToEightImmediateMode, 10) shouldEqual 0

    val lessThanEight = ArraySeq.from(List(3,9,7,9,10,9,4,9,99,-1,8))
    val lessThanEightImmediateMode = ArraySeq.from(List(3,3,1107,-1,8,3,4,3,99))

    Day5.getOutput(lessThanEight, 7) shouldEqual 1
    Day5.getOutput(lessThanEight, 10) shouldEqual 0
    Day5.getOutput(lessThanEightImmediateMode, 7) shouldEqual 1
    Day5.getOutput(lessThanEightImmediateMode, 10) shouldEqual 0

    val jumpNonZero = ArraySeq.from(List(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9))
    val jumpNonZeroImmediateMode = ArraySeq.from(List(3,3,1105,-1,9,1101,0,0,12,4,12,99,1))

    Day5.getOutput(jumpNonZero, 7) shouldEqual 1
    Day5.getOutput(jumpNonZero, -7) shouldEqual 1
    Day5.getOutput(jumpNonZero, 0) shouldEqual 0
    Day5.getOutput(jumpNonZeroImmediateMode, 7) shouldEqual 1
    Day5.getOutput(jumpNonZeroImmediateMode, -7) shouldEqual 1
    Day5.getOutput(jumpNonZeroImmediateMode, 0) shouldEqual 0

    val largeExample = ArraySeq.from(List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99))

    Day5.getOutput(largeExample, 7) shouldEqual 999
    Day5.getOutput(largeExample, 8) shouldEqual 1000
    Day5.getOutput(largeExample, 9) shouldEqual 1001

  }

}
