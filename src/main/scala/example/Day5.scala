package example

import example.Day2.extractIntCode
import example.intcode.{Operation, OperationResult, Result, SideEffectOutputResult}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day5 {

  def performComputation(intCode: ArraySeq[Int], code: Int, index: Int): Result =
    Operation.parseOperation(code, intCode, index).execute(intCode, index)

  case class IntCodeResult(intCode: ArraySeq[Int], output: Option[Int])

  def runIntCode(intCode: ArraySeq[Int], input: List[Int]): IntCodeResult = {
    @tailrec
    def compute(index: Int, updatedIntCode: ArraySeq[Int], output: Option[Int], remainingInput: List[Int]): IntCodeResult = {
      updatedIntCode(index) match {
        case 3 =>
          val position = updatedIntCode(index + 1)
          compute(index + 2, updatedIntCode.updated(position, remainingInput.head), output, remainingInput.tail)
        case 99 =>
          IntCodeResult(updatedIntCode, output)
        case opCode =>
          performComputation(updatedIntCode, opCode, index) match {
            case OperationResult(nextIndex, updatedIntCode) => compute(nextIndex, updatedIntCode, output, remainingInput)
            case SideEffectOutputResult(nextIndex, updatedIntCode, out) =>
              compute(nextIndex, updatedIntCode, Some(out), remainingInput)
          }
      }
    }
    compute(0, intCode,  None, input)
  }

  def part1(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), List(1)).output.get

  def part2(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), List(5)).output.get

  def getOutput(intCode: ArraySeq[Int], input: Int): Int = {
    val IntCodeResult(_, Some(output)) = runIntCode(intCode, List(input))
    output
  }

  def getOutput(intCode: ArraySeq[Int], input: List[Int]): Int = {
    val IntCodeResult(_, Some(output)) = runIntCode(intCode, input)
    output
  }

  def getPhaseSettingAndThrusterSignalCombinations(intCode: ArraySeq[Int], possiblePhases: List[Int]): List[(String, Int)] = for {
    phaseSetting1 <- possiblePhases
    phaseSetting2 <- possiblePhases
    phaseSetting3 <- possiblePhases
    phaseSetting4 <- possiblePhases
    phaseSetting5 <- possiblePhases

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
    println(part2Day7())
  }

  def part2Day7() =
    for {
      intCode <- extractIntCode(7)
    } yield getMaxThrusterSignalForPhase(ArraySeq.from(intCode))
}
