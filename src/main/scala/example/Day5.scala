package example

import example.Day2.extractIntCode
import example.intcode.{Operation, OperationResult, RelativeBaseOperationResult, Result, SideEffectOutputResult}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day5 {

  def performComputation(intCode: ArraySeq[Int], code: Int, index: Int, relativeBase: Int): Result =
    Operation.parseOperation(code, intCode, index, relativeBase).execute(intCode, index)

  case class IntCodeResult(intCode: ArraySeq[Int], output: Option[Int])

  def runIntCode(intCode: ArraySeq[Int], input: Int): IntCodeResult = {
    @tailrec
    def compute(index: Int, relativeBase: Int, updatedIntCode: ArraySeq[Int], output: Option[Int]): IntCodeResult = {
      updatedIntCode(index) match {
        case 3 =>
          val position = updatedIntCode(index + 1)
          compute(index + 2, relativeBase, updatedIntCode.updated(position, input), output)
        case 99 =>
          IntCodeResult(updatedIntCode, output)
        case opCode =>
          performComputation(updatedIntCode, opCode, index, relativeBase) match {
            case OperationResult(nextIndex, updatedIntCode) => compute(nextIndex, relativeBase, updatedIntCode, output)
            case SideEffectOutputResult(nextIndex, updatedIntCode, out) =>
              compute(nextIndex, relativeBase, updatedIntCode, Some(out))
            case RelativeBaseOperationResult(nextIndex, relativeBaseOffset) =>
              compute(nextIndex, relativeBase + relativeBaseOffset, updatedIntCode, output)
          }
      }
    }
    compute(0, 0, intCode,  None)
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

  def day9Part1() =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), 5).output.get

  def main(args: Array[String]): Unit = {
    runIntCode(ArraySeq.from(List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)), 0)
  }
}
