package example

import example.Day2.extractIntCode
import example.intcode.{Operation, OperationResult, Result, SideEffectOutputResult}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day5 {

  def performComputation(intCode: ArraySeq[Int], code: Int, index: Int): Result =
    Operation.parseOperation(code, intCode, index).execute(intCode, index)

  case class IntCodeResult(intCode: ArraySeq[Int], output: Option[Int])

  def runIntCode(intCode: ArraySeq[Int], input: Int): IntCodeResult = {
    @tailrec
    def compute(index: Int, updatedIntCode: ArraySeq[Int], output: Option[Int]): IntCodeResult = {
      updatedIntCode(index) match {
        case 3 =>
          val position = updatedIntCode(index + 1)
          compute(index + 2, updatedIntCode.updated(position, input), output)
        case 99 =>
          IntCodeResult(updatedIntCode, output)
        case opCode =>
          performComputation(updatedIntCode, opCode, index) match {
            case OperationResult(nextIndex, updatedIntCode) => compute(nextIndex, updatedIntCode, output)
            case SideEffectOutputResult(nextIndex, updatedIntCode, out) =>
              compute(nextIndex, updatedIntCode, Some(out))
          }
      }
    }
    compute(0, intCode,  None)
  }

  def part1(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), 1).output.get

  def part2(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), 5).output.get

  def getOutput(intCode: ArraySeq[Int], input: Int): Int = {
    val IntCodeResult(_, Some(output)) = runIntCode(intCode, input)
    output
  }
}
