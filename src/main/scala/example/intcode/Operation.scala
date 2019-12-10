package example.intcode

import scala.collection.immutable.ArraySeq

trait Operation {
  val parameters: Parameters

  def execute(intCode: ArraySeq[Int], index: Int): Result
}

trait Parameters

case class ThreeParams(v1: Int, v2: Int, v3: Int) extends Parameters
case class TwoParams(v1: Int, v2: Int) extends Parameters
case class OneParam(v1: Int) extends Parameters
case object NoParam extends Parameters

trait Result
case class OperationResult(nextIndex: Int, updatedIntCode: ArraySeq[Int]) extends Result
case class SideEffectOutputResult(nextIndex: Int, updatedIntCode: ArraySeq[Int], output: Int) extends Result

case class HaltOperation(parameters: Parameters = NoParam) extends Operation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult = OperationResult(index + 0, intCode)
}

case class Add(parameters: ThreeParams) extends Operation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult =
    OperationResult(index + 4, intCode.updated(parameters.v3, parameters.v1 + parameters.v2))
}

case class Multiply(parameters: ThreeParams) extends Operation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult =
    OperationResult(index + 4, intCode.updated(parameters.v3, parameters.v1 * parameters.v2))
}

trait JumpOperation extends Operation {
  def getInstructionPointer(params: TwoParams, index: Int, updatedIntCode: ArraySeq[Int])
                           (test: Int => Boolean) =
    if (test(params.v1)) params.v2
    else index + 3
}

case class JumpIfTrue(parameters: TwoParams) extends JumpOperation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult =
    OperationResult(getInstructionPointer(parameters, index, intCode)(x => x != 0), intCode)
}

case class JumpIfFalse(parameters: TwoParams) extends JumpOperation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult =
    OperationResult(getInstructionPointer(parameters, index, intCode)(x => x == 0), intCode)
}

trait LogicalOperation extends Operation {
  val logicalOp: (Int, Int) => Boolean

  def performLogicalOp(updatedIntCode: ArraySeq[Int], params: ThreeParams): ArraySeq[Int] = {
    if (logicalOp(params.v1, params.v2))
      updatedIntCode.updated(params.v3, 1)
    else
      updatedIntCode.updated(params.v3, 0)
  }
}

case class LessThan(parameters: ThreeParams,
                    logicalOp: (Int, Int) => Boolean) extends LogicalOperation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult =
    OperationResult(index + 4, performLogicalOp(intCode, parameters))
}

case class Equals(parameters: ThreeParams,
                  logicalOp: (Int, Int) => Boolean) extends Operation with LogicalOperation {
  override def execute(intCode: ArraySeq[Int], index: Int): OperationResult =
    OperationResult(index + 4, performLogicalOp(intCode, parameters))
}

case class Output(parameters: OneParam) extends Operation {
  override def execute(intCode: ArraySeq[Int], index: Int): SideEffectOutputResult = {
    println(parameters.v1)
    SideEffectOutputResult(index + 2, intCode, parameters.v1)
  }
}

object Operation {

  val IMMEDIATE_MODE = 1
  val POSITION_MODE = 0

  def parseOperation(ins: Int, intCode: ArraySeq[Int], index: Int): Operation = {
    val opCode = ins % 100

    if (opCode == 99) {
      return HaltOperation()
    }

    val mode1 = ins / 100 % 10
    val mode2 = ins / 1000 % 10

    if (List(1, 2, 7, 8) contains opCode) {
      val params: ThreeParams = ThreeParams(
        getValue(mode1, intCode, index + 1),
        getValue(mode2, intCode, index + 2),
        intCode(index + 3))
      opCode match {
        case 1 => Add(params)
        case 2 => Multiply(params)
        case 7 => LessThan(params, (x, y) => x < y)
        case 8 => Equals(params, (x, y) => x == y)
      }
    } else if (List(5, 6) contains opCode) {
      val params: TwoParams = TwoParams(
        getValue(mode1, intCode, index + 1),
        getValue(mode2, intCode, index + 2))
      opCode match {
        case 5 => JumpIfTrue(params)
        case 6 => JumpIfFalse(params)
      }
    }
    else {
      val param = OneParam(getValue(mode1, intCode, index + 1))
      opCode match {
        case 4 => Output(param)
      }
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
