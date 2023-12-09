package code.day3

import scala.io.Source
import scala.util.Using
import java.util.Map.Entry
import ujson.Num

object Day3 {
  val dir: List[(Int, Int)] =
    List((1, 1), (1, 0), (0, 1), (1, -1), (-1, 1), (0, -1), (-1, 0), (-1, -1))
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }

  def partOne(input: List[String]): Int = {
    var allNums: List[Number] = input.zipWithIndex.flatMap(getNums)
    val valMapping: Map[Position, Int] =
      allNums.map(n => n.start -> n.value).toMap
    var parts: Set[Number] = Set()
    val charPos = for
      (line, i) <- input.zipWithIndex
      (char, j) <- line.zipWithIndex
      if !char.isDigit && char != '.'
    yield (i, j)

    getSurroundingNums(charPos, input)
      .map(pos => Number(pos, valMapping.get(pos).get))
      .toSet
      .toList
      .map(_._2)
      .sum

  }

  def getSurroundingNums(
      positions: List[Position],
      input: List[String]
  ): List[Position] = {
    (for
      pos <- positions
      move <- dir
    yield
      var x = pos._1 + move._1
      var y = pos._2 + move._2
      val numRows = input.length
      val numCols = input(0).length()
      var res: List[Position] = List()
      if x >= 0 && y >= 0 && x < numRows && y < numCols && input(x)(y).isDigit
      then
        while y >= 0 && input(x)(y).isDigit do y = y - 1
        (x, y + 1)
      else (-1, -1)
    ).filter(_ != (-1, -1)).toSet.toList

  }

  // get the starting position of all numbers in a line
  def getNums(line: String, lineNum: Int): List[Number] =
    val pattern = "\\d+".r
    pattern
      .findAllMatchIn(line)
      .map(m => Number((lineNum, m.start), m.matched.toInt))
      .toList

  case class Number(start: (Int, Int), value: Int)
  type Position = (Int, Int)

  def partTwo(input: List[String]): Int = {
    var allNums: List[Number] = input.zipWithIndex.flatMap(getNums)
    val valMapping: Map[Position, Int] =
      allNums.map(n => n.start -> n.value).toMap

    var signs = for
      (line, i) <- input.zipWithIndex
      (char, j) <- line.zipWithIndex
      if char == '*'
    yield (i, j)

    signs
      .map(pos => getSurroundingNums(List(pos), input))
      .toSet
      .toList
      .filter(_.length == 2)
      .map(_.map(p => valMapping.get(p).get).fold(1)(_ * _))
      .sum
  }

  @main def main(): Unit = {
    val inputLines1: List[String] = readInput(
      "src/main/scala/code/day3/day3.txt"
    )
    val inputLines2: List[String] = readInput(
      "src/main/scala/code/day3/day3_2.txt"
    )
    // println(s"Part One: ${partOne(inputLines1)}")
    println(s"Part Two: ${partTwo(inputLines2)}")
  }
}
