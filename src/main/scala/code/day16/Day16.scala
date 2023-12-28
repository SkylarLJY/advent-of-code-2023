package code.day16

import scala.io.Source
import scala.util.Using

object Day16 {
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }

  enum Direction(val value: (Int, Int), val str: String) {
    case GoRight extends Direction((0, 1), "r")
    case GoLeft extends Direction((0, -1), "l")
    case GoUp extends Direction((-1, 0), "u")
    case GoDown extends Direction((1, 0), "d")
  }

  def partOne(input: List[String]): Int = {
    // visited keep from which direction a position has been visited
    // -> terminat if it's a loop
    var visited = Array.fill(input.length, input(0).length)("")
    move((0, 0), Direction.GoRight, input, visited)
    visited.flatten.filter(_.nonEmpty).length
  }

  def move(pos: (Int, Int), dir: Direction, input: List[String], visited: Array[Array[String]]): Unit = {
    if pos._1 < 0 || pos._1 >= input.length || pos._2 < 0 || pos._2 >= input(0).length
    then return
    if visited(pos._1)(pos._2).contains(dir.str) then return
    visited(pos._1)(pos._2) = visited(pos._1)(pos._2) + dir.str

    val newDirs: List[Direction] = input(pos._1)(pos._2) match
      case '|' if dir == Direction.GoLeft || dir == Direction.GoRight =>
        List(Direction.GoUp, Direction.GoDown)
      case '-' if dir == Direction.GoDown || dir == Direction.GoUp =>
        List(Direction.GoRight, Direction.GoLeft)
      case '/' if dir == Direction.GoRight  => List(Direction.GoUp)
      case '/' if dir == Direction.GoLeft   => List(Direction.GoDown)
      case '/' if dir == Direction.GoUp     => List(Direction.GoRight)
      case '/' if dir == Direction.GoDown   => List(Direction.GoLeft)
      case '\\' if dir == Direction.GoLeft  => List(Direction.GoUp)
      case '\\' if dir == Direction.GoRight => List(Direction.GoDown)
      case '\\' if dir == Direction.GoDown  => List(Direction.GoRight)
      case '\\' if dir == Direction.GoUp    => List(Direction.GoLeft)
      // ., - dir=l/r, | dir = u/d
      case _ => List(dir)

    for d <- newDirs do
      move((pos._1 + d.value._1, pos._2 + d.value._2), d, input, visited)
  }

  case class PosDir(pos: (Int, Int), dir: Direction)

  def partTwo(input: List[String]): Int = {
    val row = input.length
    val col = input(0).length
    var visitedEdge = List.empty[(Int, Int)]
    val startingPoints = 
        for i <- 0 until row
            j <- 0 until col
            if i==0 || j==0 || i==row-1 || j==col-1
        yield (i, j)

    val startingPointsWithDir = startingPoints.flatMap{
        case (0, 0) => List(PosDir((0,0), Direction.GoDown), PosDir((0,0), Direction.GoRight))
        case (0, j) if j==col-1 => List(PosDir((0,j), Direction.GoDown), PosDir((0,j), Direction.GoLeft)) 
        case (i, 0) if i==row-1 => List(PosDir((i,0), Direction.GoUp), PosDir((i,0), Direction.GoRight))
        case (i, j) if i==row-1 && j==col-1 => List(PosDir((i, j), Direction.GoUp), PosDir((i, j), Direction.GoLeft))
        case (0, j) => List(PosDir((0, j), Direction.GoDown))
        case (i, 0) => List(PosDir((i, 0), Direction.GoRight)) 
        case (i, j) if i==row-1 => List(PosDir((i, j), Direction.GoUp))
        case (i, j) if j==col-1 => List(PosDir((i, j), Direction.GoLeft))
    }.toList

    var maxCoverage = 0
    for posDir <- startingPointsWithDir do
        var visited = Array.fill(input.length, input(0).length)("")
        move(posDir.pos, posDir.dir, input, visited)
        maxCoverage = Math.max(maxCoverage, visited.flatten.filter(_.nonEmpty).length)
    maxCoverage
  }

  @main def main(): Unit = {
    val inputLines1: List[String] = readInput("src/main/scala/code/day16/day16.txt")
    val inputLines2: List[String] = readInput("src/main/scala/code/day16/day16_2.txt")
    println(s"Part One: ${partOne(inputLines1)}")
    println(s"Part Two: ${partTwo(inputLines2)}")
  }
}
