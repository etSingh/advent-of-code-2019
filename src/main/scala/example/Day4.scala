package example

import scala.annotation.tailrec

object Day4 {

  def hasAtLeastTwoSameAdjacentDigits(number: Int): Boolean = {
    @tailrec
    def find(current: Int, remaining: Int): Boolean = {
      if (remaining % 10 == current) true
      else if (remaining == 0) false
      else find(remaining % 10, remaining / 10)
    }
    find(number % 10, number / 10)
  }

  def hasAtMostTwoSameAdjacentDigits(number: Int): Boolean =
     checkSmallestRepeatingGroup(number % 10, number / 10, 1)

  @tailrec
  def checkSmallestRepeatingGroup(current: Int, remaining: Int, currentMatching: Int): Boolean = {
    val next = remaining % 10

    if (next == current) checkSmallestRepeatingGroup(next, remaining / 10, currentMatching + 1)
    else if (remaining == 0) currentMatching == 2
    else if (next != current && currentMatching == 2) true
    else checkSmallestRepeatingGroup(remaining % 10, remaining / 10, 1)
  }

  def digitsDecreaseOrEqualToWhenGoingFromRightToLeft(number: Int): Boolean = {
    def check(current: Int, remaining: Int): Boolean = {
      if (remaining == 0) true
      else if (remaining % 10 > current) false
      else check(remaining % 10, remaining / 10)
    }
    check(number % 10, number / 10)
  }

  def matchesAtLeastCriteria(number: Int): Boolean =
   hasAtLeastTwoSameAdjacentDigits(number) && digitsDecreaseOrEqualToWhenGoingFromRightToLeft(number)

  def matchesAtMostCriteria(number: Int): Boolean =
     digitsDecreaseOrEqualToWhenGoingFromRightToLeft(number) && hasAtMostTwoSameAdjacentDigits(number)

  def compute(start: 136818, end: 685979, matcher: Int => Boolean): Int = {
    def findTotalMatching(current: Int, matching: Int): Int =
      if (current == end) matching
      else if (matcher(current)) findTotalMatching(current + 1, matching + 1)
      else findTotalMatching(current + 1, matching)

    findTotalMatching(start, 0)
  }
}
