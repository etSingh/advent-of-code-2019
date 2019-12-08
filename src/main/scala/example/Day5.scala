package example

import example.IntCode.{extractIntCode, extractPositions}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day5 {

  val IMMEDIATE_MODE = 1
  val POSITION_MODE = 0

  case class Values(v1: Int, v2: Int, v3: Int)

  case class Operation(opCode: Int, values: Option[Values])

  object HaltOperation extends Operation(opCode = 99, values = None)

  object Operation {
    def parseOperation(ins: Int, intCode: ArraySeq[Int], index: Int): Operation = {
      val opCode = ins % 100

      if (opCode == 99) {
        return HaltOperation
      }

      val mode1 = ins / 100 % 10
      val mode2 = ins / 1000 % 10

      if (opCode == 1 || opCode == 2) {
        Operation(opCode, Some(Values(
          getValue(mode1, intCode, index + 1),
          getValue(mode2, intCode, index + 2),
          intCode(index + 3))))
      } else {
        val value = getValue(mode1, intCode, index + 1)
        Operation(opCode, Some(
          Values(value, value, value))) // FIXME
      }
    }

    def getValue(mode: Int, intCode: ArraySeq[Int], index: Int): Int = {
      if (mode == IMMEDIATE_MODE) {
        intCode(index)
      } else {
        val position = intCode(index)
        intCode(position)
      }
    }
  }

  def performComputation(intCode: ArraySeq[Int], code: Int, index: Int): (Int, ArraySeq[Int]) =
    Operation.parseOperation(code, intCode, index) match {
      case HaltOperation => (0, intCode)
      case Operation(opCode, Some(values)) =>
        opCode match {
          case 1 => (4, intCode.updated(values.v3, values.v1 + values.v2))
          case 2 => (4, intCode.updated(values.v3, values.v1 * values.v2))
          case 4 =>
            println(values.v1)
            (2, intCode)
        }
    }

  def runIntCode(intCode: ArraySeq[Int], input: Int): ArraySeq[Int] = {
    @tailrec
    def compute(index: Int, updatedIntCode: ArraySeq[Int]): ArraySeq[Int] = {
      updatedIntCode(index) match {
        case 1 =>
          val positions = extractPositions(updatedIntCode, index)
          val element = updatedIntCode(positions.pos1) + updatedIntCode(positions.pos2)
          compute(index + 4, updatedIntCode.updated(positions.pos3, element))
        case 2 =>
          val positions = extractPositions(updatedIntCode, index)
          val element = updatedIntCode(positions.pos1) * updatedIntCode(positions.pos2)
          compute(index + 4, updatedIntCode.updated(positions.pos3, element))
        case 3 =>
          val position = updatedIntCode(index + 1)
          compute(index + 2, updatedIntCode.updated(position, input))
        case 4 =>
          val value = updatedIntCode(index + 1)
          println(updatedIntCode(value))
          compute(index + 2, updatedIntCode)
        case 99 =>
          updatedIntCode
        case opCode =>
          val (i, update) = performComputation(updatedIntCode, opCode, index)
          compute(index + i, update)
      }
    }

    compute(0, intCode)
  }

  def part1(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), 1).toList.head


  def main(args: Array[String]): Unit = {
    print(part1())
  }

}
