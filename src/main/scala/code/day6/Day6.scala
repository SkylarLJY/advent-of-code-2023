package code.day6

import scala.io.Source
import scala.util.Using

object Day6 {
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }

  def partOne(input: List[String]): Int =
    input(0)
      .split("\\s+")
      .tail
      .map(_.toLong)
      .zip(input(1).split("\\s+").tail.map(_.toLong))
      .map(boatRace)
      .reduce(_*_)

  def boatRace(time: Long, dist: Long): Int = {
    // holdTime * (time - holdTime) > distance
    // h^2 - h*t + d < 0
    val discriminant = time * time - 4 * dist 
    val root1 = math.ceil((time + math.sqrt(discriminant)) / 2) - 1
    val root2 = math.floor((time - math.sqrt(discriminant)) / 2) + 1
    math.abs(root2-root1).toInt + 1

  }

  def partTwo(input: List[String]): Int =
    input.map(_.split("\\s+").tail.reduce(_+_).toLong) match
        case List(time, dist) => boatRace(time, dist)

  @main def main(): Unit = {
    val inputLines: List[String] = readInput(
      "src/main/scala/code/day6/day6.txt"
    )

    // println(s"Part One: ${partOne(inputLines)}")
    println(s"Part Two: ${partTwo(inputLines)}")
  }
}
