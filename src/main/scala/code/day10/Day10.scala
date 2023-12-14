package code.day10

import scala.io.Source
import scala.util.Using

object Day10 {
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }
  val goUp = (-1, 0)
  val goDown = (1, 0)
  val goLeft = (0, -1)
  val goRight = (0, 1)
  val dir = List(goUp, goRight, goLeft, goDown)

  def partOne(input: List[String]): Int =
    // enter & leave on different ends
    // 2d array to keep dist
    val rows = input.length
    val cols = input(0).length
    val dist: Array[Array[Int]] = Array.ofDim[Int](rows, cols)
    for {
      i <- 0 until rows
      j <- 0 until cols
    } dist(i)(j) = -1
    val start = (for
      i <- 0 until rows
      j <- 0 until cols
      if input(i)(j) == 'S'
    yield (i, j))(0)

    dist(start._1)(start._2) = 0
    for d <- 0 to 3 do walk(start, dir(d), input, dist)
    dist.map(_.max).max

  def walk(
      pos: (Int, Int),
      direction: (Int, Int),
      map: List[String],
      dist: Array[Array[Int]]
  ): Unit =
    val i = pos._1 + direction._1
    val j = pos._2 + direction._2
    val rows = map.length
    val cols = map(0).length
    if i < 0 || j < 0 || i >= rows || j >= rows then
      return // return if out of bound
    val currStep = dist(pos._1)(pos._2)
    if (dist(i)(j) != -1 && dist(i)(j) < currStep+1)
      return // return if can get there with less steps than this route

    map(i)(j) match
      case c
          if (c == 'F' && direction == goUp) || (c == 'L' && direction == goDown) || (c == '-' && direction == goRight) =>
        dist(i)(j) = currStep + 1
        walk((i, j), goRight, map, dist)
      case c
          if (c == '-' && direction == goLeft) || (c == 'J' && direction == goDown) || (c == '7' && direction == goUp) =>
        dist(i)(j) = currStep + 1
        walk((i, j), goLeft, map, dist)
      case c
          if (c == '|' && direction == goUp) || (c == 'L' && direction == goLeft) || (c == 'J' && direction == goRight) =>
        dist(i)(j) = currStep + 1
        walk((i, j), goUp, map, dist)
      case c
          if (c == '|' && direction == goDown) || (c == 'F' && direction == goLeft) || (c == '7' && direction == goRight) =>
        dist(i)(j) = currStep + 1
        walk((i, j), goDown, map, dist)
      case _ => return

  def partTwo(input: List[String]): Int = ???

  @main def main(): Unit = {
    val inputLines1: List[String] = readInput(
      "src/main/scala/code/day10/day10.txt"
    )
    val inputLines2: List[String] = readInput(
      "src/main/scala/code/day10/day10_2.txt"
    )
    println(s"Part One: ${partOne(inputLines1)}")
    // println(s"Part Two: ${partTwo(inputLines2)}")
  }
}
