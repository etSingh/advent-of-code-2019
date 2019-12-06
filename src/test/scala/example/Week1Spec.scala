package example

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
    Day2.part1().map(
      ans => ans shouldEqual 3058646
    )

    Day2.part2().map(
      ans => ans.head shouldEqual 8976
    )
  }
}
