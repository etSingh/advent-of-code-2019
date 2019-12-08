package example

import example.IntCode.{extractIntCode, extractPositions}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day5 {

  val IMMEDIATE_MODE = 1
  val POSITION_MODE = 0

  trait Parameters

  case class ThreeParams(v1: Int, v2: Int, v3: Int) extends Parameters
  case class TwoParams(v1: Int, v2: Int) extends Parameters
  case class OneParam(v1: Int) extends Parameters

  case class Operation(opCode: Int, values: Option[Parameters])
  object HaltOperation extends Operation(opCode = 99, values = None)

  object Operation {
    def parseOperation(ins: Int, intCode: ArraySeq[Int], index: Int): Operation = {
      val opCode = ins % 100

      if (opCode == 99) {
        return HaltOperation
      }

      val mode1 = ins / 100 % 10
      val mode2 = ins / 1000 % 10

      if (List(1, 2, 7, 8) contains opCode) {
        Operation(opCode, Some(ThreeParams(
          getValue(mode1, intCode, index + 1),
          getValue(mode2, intCode, index + 2),
          intCode(index + 3))))
      } else if (List(5, 6) contains opCode){
        Operation(opCode, Some(TwoParams(
          getValue(mode1, intCode, index + 1),
          getValue(mode2, intCode, index + 2))))
      }
      else {
        val value = getValue(mode1, intCode, index + 1)
        Operation(opCode, Some(OneParam(value)))
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

    def performLogicalOp(updatedIntCode: ArraySeq[Int], params: ThreeParams)
                        (logicalOp: (Int, Int) => Boolean): ArraySeq[Int] = {
      if (logicalOp(params.v1, params.v2))
        updatedIntCode.updated(params.v3, 1)
      else
        updatedIntCode.updated(params.v3, 0)
    }

    def getInstructionPointer(params: TwoParams, index: Int, updatedIntCode: ArraySeq[Int])
                             (test: Int => Boolean) =
      if (test(params.v1)) params.v2
      else index + 3

  }

  def performComputation(intCode: ArraySeq[Int], code: Int, index: Int): (Int, ArraySeq[Int]) =
    Operation.parseOperation(code, intCode, index) match {
      case HaltOperation => (index + 0, intCode)
      case Operation(opCode, Some(params: ThreeParams)) =>
        opCode match {
          case 1 => (index + 4, intCode.updated(params.v3, params.v1 + params.v2))
          case 2 => (index + 4, intCode.updated(params.v3, params.v1 * params.v2))
          case 7 => (index + 4, Operation.performLogicalOp(intCode, params)((x, y) => x < y))
          case 8 => (index + 4, Operation.performLogicalOp(intCode, params)((x, y) => x == y))
        }
      case Operation(4, Some(param: OneParam)) =>
        output = param.v1
        println(param.v1)
        (index + 2, intCode)
      case Operation(opCode, Some(params: TwoParams)) =>
        opCode match {
          case 5 => (Operation.getInstructionPointer(params, index, intCode)(x => x != 0), intCode)
          case 6 => (Operation.getInstructionPointer(params, index, intCode)(x => x == 0), intCode)
        }
    }

  def getInstructionPointer(index: Int, updatedIntCode: ArraySeq[Int], test: Int => Boolean) = {
    val firstParameter = updatedIntCode(index + 1)
    if (test(updatedIntCode(firstParameter))) {
      val secondParameter = updatedIntCode(index + 2)
      Some(updatedIntCode(secondParameter))
    } else {
      None
    }
  }

  def performLogicalOp(updatedIntCode: ArraySeq[Int], index: Int, logicalOp: (Int, Int) => Boolean): ArraySeq[Int] = {
    val firstParameter = updatedIntCode(index + 1)
    val secondParameter = updatedIntCode(index + 2)
    val thirdParameter = updatedIntCode(index + 3)
    if (logicalOp(updatedIntCode(firstParameter), updatedIntCode(secondParameter)))
      updatedIntCode.updated(thirdParameter, 1)
    else
      updatedIntCode.updated(thirdParameter, 0)
  }

  var output = Integer.MAX_VALUE

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
          output = updatedIntCode(value)
          println(updatedIntCode(value))
          compute(index + 2, updatedIntCode)
        case 5 =>
          getInstructionPointer(index, updatedIntCode, x => x != 0) match {
            case Some(pointer) => compute(pointer, updatedIntCode)
            case None => compute(index + 3, updatedIntCode)
          }
        case 6 => getInstructionPointer(index, updatedIntCode, x => x == 0) match {
          case Some(pointer) => compute(pointer, updatedIntCode)
          case None => compute(index + 3, updatedIntCode)
        }
        case 7 =>
          compute(index + 4, performLogicalOp(updatedIntCode, index, (x: Int, y: Int) => x < y))
        case 8 =>
          compute(index + 4, performLogicalOp(updatedIntCode, index, (x: Int, y: Int) => x == y))
        case 99 =>
          updatedIntCode
        case opCode =>
          val (newIndex, update) = performComputation(updatedIntCode, opCode, index)
          compute(newIndex, update)
      }
    }

    compute(0, intCode)
  }

  def part1(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), 1).toList.head

  def part2(): Either[String, Int] =
    for {
      intCode <- extractIntCode(5)
    } yield runIntCode(ArraySeq.from(intCode), 5).toList.head


  def getOutput(intCode: ArraySeq[Int], input: Int): Int = {
    val res = runIntCode(intCode, input)
    output // Dirty hack
  }


}
