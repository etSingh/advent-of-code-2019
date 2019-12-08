package example

import example.util.HttpClient

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

// Started from Day 2
object IntCode {

  case class Positions(pos1: Int, pos2: Int, pos3: Int)

  def extractPositions(intCode: ArraySeq[Int], index: Int) = {
    val pos1 = intCode(index + 1)
    val pos2 = intCode(index + 2)
    val pos3 = intCode(index + 3)
    Positions(pos1, pos2, pos3)
  }

  def runIntCode(intCode: ArraySeq[Int]): ArraySeq[Int] = {
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
        case 99 =>
          updatedIntCode
      }
    }
    compute(0, intCode)
  }

  def modifyCode(intCode: ArraySeq[Int], code1: Int, code2: Int): ArraySeq[Int] = {
    val modifiedIntCode = intCode.updated(1, code1).updated(2, code2)
    modifiedIntCode
  }

  def part1(): Either[String, Int] =
    for {
      intCode <- extractIntCode(2)
      modifiedCode = modifyCode(ArraySeq.from(intCode), 12, 2)
    } yield runIntCode(modifiedCode).toList.head

  def part2(): Either[String, IndexedSeq[Int]] = {
    val result = for {
      intCode <- extractIntCode(2)
      intCodeImmutable = ArraySeq.from(intCode)
    } yield
      for {
        num1 <- 0 to 99
        num2 <- 0 to 99
        output = runIntCode(modifyCode(intCodeImmutable, num1, num2)).head
        res = checkIfGotMatchingOutput(output, num1, num2)
      } yield res

    result
      .flatMap(
        r => Right(
          r.filter(v => v.isDefined)
            .map(_.get)))
  }

  def checkIfGotMatchingOutput(output: Int, num1: Int, num2: Int): Option[Int] =
    if (output == 19690720) Some(100 * num1 + num2) else None

  def validateResponse(resp: String): String = {
    val lastIndex = resp.length - 1
    if (resp(lastIndex) == '\n')
      resp.substring(0, lastIndex)
    else
      resp
  }

  def extractIntCode(day: Int): Either[String, Array[Int]] = {
      for {
        resp <- HttpClient.get(s"https://adventofcode.com/2019/day/$day/input")
        validResp = validateResponse(resp)
        ln = validResp.split(",")
      } yield ln.map(_.toInt)
  }
}