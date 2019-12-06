package example

import example.util.HttpClient

object Day3 {

  case class Coordinates(x: Int, y: Int)
  case class CoordinatesAndDistance(c: Coordinates, totalDistance: Int)

  case class WirePath(direction: Char, steps: Int)

  object WirePath {
    def build(path: String): WirePath = WirePath(path.head, path.substring(1).toInt)
    def buildMultiple(paths: String): List[WirePath] = paths.split(",").map(path => build(path)).toList
  }

  val moveRight: Coordinates => Coordinates = c => Coordinates(c.x + 1, c.y)
  val moveLeft: Coordinates => Coordinates = c => Coordinates(c.x - 1, c.y)
  val moveUp: Coordinates => Coordinates = c => Coordinates(c.x, c.y + 1)
  val moveDown: Coordinates => Coordinates = c => Coordinates(c.x, c.y - 1)

  def expandPath(steps: Int, start: Coordinates, move: Coordinates => Coordinates): List[Coordinates] = {
    def expand(remainingSteps: Int, current: Coordinates, path: List[Coordinates]): List[Coordinates] =
      if (remainingSteps == 0)
        path
      else
        expand(remainingSteps - 1, move(current), path ++ List(move(current)))
    expand(steps, start, List())
  }

  def expandCoordinates(wirePath: WirePath, start: Coordinates): List[Coordinates] = {
    wirePath.direction match {
      case 'R' => expandPath(wirePath.steps, start, moveRight)
      case 'L' => expandPath(wirePath.steps, start, moveLeft)
      case 'U' => expandPath(wirePath.steps, start, moveUp)
      case 'D' => expandPath(wirePath.steps, start, moveDown)
    }
  }

  def convertPathToCoordinates(wirePaths: List[WirePath], start: Coordinates): List[Coordinates] = {
    if (wirePaths.isEmpty)
      Nil
    else {
      val nextSegment = expandCoordinates(wirePaths.head, start)
      nextSegment ++ convertPathToCoordinates(wirePaths.tail, nextSegment.last)
    }
  }

  def findMatchingCoordinates(line1: List[Coordinates], line2: List[Coordinates]): List[Coordinates] = {
    println("Finding intersections...")
    for {
      l1 <- line1
      l2 <- line2
      if l1 == l2
    } yield l1
  }

  def findMatchingCoordinatesWithDistance(line1: List[Coordinates], line2: List[Coordinates]):
  List[CoordinatesAndDistance] = {
    println("Finding intersections...")
    for {
      l1 <- line1
      l2 <- line2
      if l1 == l2
    } yield CoordinatesAndDistance(l1, line1.indexOf(l1) + line2.indexOf(l2) + 2)
  }

  def calculateSmallestManhattanDistance(intersections: List[Coordinates]): Int =
    intersections.map(c => Math.abs(c.x) + Math.abs(c.y)).min

  def calculateSmallestDistance(intersections: List[CoordinatesAndDistance]): Int =
    intersections.map(_.totalDistance).min

  def extractWirePaths(): Either[String, List[String]] =
    for {
      resp <- HttpClient.get("https://adventofcode.com/2019/day/3/input")
      ln = resp.split("\n")
    } yield ln.toList

  def part1(): Either[String, Int] = {
    val lines = extractWirePaths()
    val start = Coordinates(0, 0)
    for {
      ln <- lines
      c1 = convertPathToCoordinates(WirePath.buildMultiple(ln.head), start)
      c2 = convertPathToCoordinates(WirePath.buildMultiple(ln.tail.head), start)
      intersections = findMatchingCoordinates(c1, c2)
      out3 = println("Intersections found!")
    } yield calculateSmallestManhattanDistance(intersections)
  }

  def part2(): Either[String, Int] = {
    val lines = extractWirePaths()
    val start = Coordinates(0, 0)
    for {
      ln <- lines
      c1 = convertPathToCoordinates(WirePath.buildMultiple(ln.head), start)
      c2 = convertPathToCoordinates(WirePath.buildMultiple(ln.tail.head), start)
      intersections = findMatchingCoordinatesWithDistance(c1, c2)
      out3 = println("Intersections found!")
    } yield calculateSmallestDistance(intersections)
  }

  def main(args: Array[String]): Unit = {
    println(part1())
    println(part2())
  }
}
