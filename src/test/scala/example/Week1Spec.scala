package example

import example.Day4.{compute, matchesAtMostCriteria, matchesAtLeastCriteria}
import org.scalatest._

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
}
