package example

import example.Day2.extractIntCode
import example.Day5.{IntCodeResult, runIntCode}

import scala.collection.immutable.ArraySeq

object Day7 {


  def getOutput(intCode: ArraySeq[Int], input: List[Int]): Int = {
    val IntCodeResult(_, Some(output)) = runIntCode(intCode, input)
    output
  }

  def getPhaseSettingAndThrusterSignalCombinations(intCode: ArraySeq[Int], possiblePhases: List[Int]): List[(String, Int)] = for {
    phaseSetting1 <- possiblePhases
    phaseSetting2 <- possiblePhases.filter(phase => phase != phaseSetting1)
    phaseSetting3 <- possiblePhases.filter(phase => phase != phaseSetting1 && phase != phaseSetting2)
    phaseSetting4 <- possiblePhases.filter(phase => phase != phaseSetting1 && phase != phaseSetting2 && phase != phaseSetting3)
    phaseSetting5 <- possiblePhases.filter(phase => phase != phaseSetting1 && phase != phaseSetting2 && phase != phaseSetting3 && phase != phaseSetting4)

    output1 = getOutput(intCode, List(phaseSetting1, 0))
    output2 = getOutput(intCode, List(phaseSetting2, output1))
    output3 = getOutput(intCode, List(phaseSetting3, output2))
    output4 = getOutput(intCode, List(phaseSetting4, output3))
    output5 = getOutput(intCode, List(phaseSetting5, output4))
  } yield (f"$phaseSetting1$phaseSetting2$phaseSetting3$phaseSetting4$phaseSetting5", output5)

  def getMaxThrusterSignalForPhase(intCode: ArraySeq[Int]): (String, Int) = {
    val POSSIBLE_PHASES = List(0, 1, 2, 3, 4)
    getPhaseSettingAndThrusterSignalCombinations(intCode, POSSIBLE_PHASES).max(Ordering.by[(String, Int), Int](_._2))
  }


  def main(args: Array[String]): Unit = {
    println(part1())
  }

  def part1() =
    for {
      intCode <- extractIntCode(7)
    } yield getMaxThrusterSignalForPhase(ArraySeq.from(intCode))

}
