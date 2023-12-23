package code.day13

import scala.io.Source
import scala.util.Using

object Day13 {
  def readInput(fileName: String): List[List[String]] =
    Source
      .fromFile(fileName)
      .mkString
      .split("\n\n")
      .toList
      .map(_.trim.split("\n").toList)

  def partOne(input: List[List[String]]): Int = {
    input.map(findMirrorVals).sum
  }

  def findMirrorVals(pattern: List[String]): Int = {
    // try find horizontal mirror, if not found try vertical mirror
    val vertical =
      (for line <- pattern
      yield mirrorIndeces(line)).reduce((a, b) => a.intersect((b)))
    if !vertical.isEmpty then return vertical(0)

    val horizontal =
      (for line <- pattern.transpose
      yield mirrorIndeces(line.mkString)).reduce((a, b) => a.intersect(b))
    if !horizontal.isEmpty then return horizontal(0) * 100
    -1
  }

  def mirrorIndeces(line: String): List[Int] = {
    (for {
      i <- 0 until line.length - 1
      len = Math.min(i + 1, line.length() - i - 1)
      frontFlipped = line.substring(0, i + 1).reverse.substring(0, len)
      back = line.substring(i + 1).substring(0, len)
      if frontFlipped == back
    } yield i + 1).toList
  }

  def partTwo(input: List[List[String]]): Int = {
    input.map(findMirrorWithSmudge).sum
  }

  def findMirrorWithSmudge(pattern: List[String]): Int = {
    val vertical =
      (for line <- pattern
      yield mirrorIndeces(line)).flatten.groupBy(identity).mapValues(_.size).filter{
        case (_, count) => count == pattern.length-1
      }.keys.toList
    if !vertical.isEmpty then return vertical(0)

    val horizontal =
      (for line <- pattern.transpose
      yield mirrorIndeces(line.mkString)).flatten.groupBy(identity).mapValues(_.size).filter{
        case (_, count) => count == pattern(0).length-1
      }.keys.toList
    if !horizontal.isEmpty then return horizontal(0) * 100
    -1
  }

  @main def main(): Unit = {
    val inputLines1: List[List[String]] = readInput(
      "src/main/scala/code/day13/day13.txt"
    )
    val inputLines2: List[List[String]] = readInput("src/main/scala/code/day13/day13_2.txt")
    // println(s"Part One: ${partOne(inputLines1)}")
    println(s"Part Two: ${partTwo(inputLines2)}")
  }
}
