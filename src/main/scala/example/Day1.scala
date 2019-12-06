package example

import example.util.HttpClient

import scala.annotation.tailrec

object Day1 {

  def getFuelPerModule(moduleMass: Double): Double = {
    @tailrec
    def calculateFuel(fuels: Double, mass: Double): Double = {
      val fuel = getFuel(mass)
      if (fuel <= 0) fuels
      else calculateFuel(fuels + fuel, fuel)
    }
    calculateFuel(0, moduleMass)
  }

  def part2(): Either[String, Int] =
    extractMass() match {
      case Left(value) => Left(value)
      case Right(moduleMasses) =>
        val res = (for {
          moduleMass <- moduleMasses
        } yield getFuelPerModule(moduleMass)).sum.toInt
        Right(res)
    }

  def part1(): Either[String, Int] =
    for {
      moduleMasses <- extractMass()
    } yield calculateFuelRequirements(moduleMasses)

  def calculateFuelRequirements(moduleMasses: List[Double]): Int = {
    val fuelRequirements: List[Double] =
      for {
        mass <- moduleMasses
      } yield getFuel(mass)
    fuelRequirements.sum.toInt
  }

  def getFuel(mass: Double): Double = {
    val dividedByThree = mass / 3
    val roundDown = java.lang.Math.floor(dividedByThree)
    roundDown - 2
  }

  def extractMass(): Either[String, List[Double]] = {
    for {
      resp <- HttpClient.get("https://adventofcode.com/2019/day/1/input")
      ln = resp.split("\n")
    } yield ln.map(_.toDouble).toList
  }

}