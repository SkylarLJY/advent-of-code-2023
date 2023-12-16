package code.day11

import scala.io.Source
import scala.util.Using

object Day11 {
  def readInput(fileName: String): Array[Array[Char]] =
    Source.fromFile(fileName).getLines().map(_.toCharArray()).toArray

  var expansionFactor = 1;
  def partOne(input: Array[Array[Char]]): Long = {
    val emptyRows = input.zipWithIndex.collect {
      case (row, i) if row.distinct.length == 1 => i
    }.toList
    val emptyCols = input.transpose.zipWithIndex.collect {
      case (col, i) if col.distinct.length == 1 => i
    }.toList
    val row = input.length
    val col = input(0).length
    val galaxies =
      for
        i <- 0 until row
        j <- 0 until col
        if input(i)(j) == '#'
      yield (i, j)

    (for
      (x1, y1) <- galaxies
      (x2, y2) <- galaxies
      if !(x1 == x2 && y1 == y2) 
    yield getDist((x1, y1), (x2, y2), emptyRows, emptyCols)).sum/2
  }

  def getDist(
      pos1: (Long, Long),
      pos2: (Long, Long),
      expandedRows: List[Int],
      expandedCols: List[Int]
  ): Long = {
    val expandedRowCount = expandedRows
      .filter(rowNum =>
        rowNum > Math.min(pos1._1, pos2._1) && rowNum < Math
          .max(pos1._1, pos2._1)
      )
      .length
    val expandedColCount = expandedCols
      .filter((colNum) =>
        colNum > Math.min(pos1._2, pos2._2) && colNum < Math
          .max(pos1._2, pos2._2)
      )
      .length
    Math.abs(pos1._1 - pos2._1) + Math.abs(
      pos1._2 - pos2._2
    ) + (expandedColCount + expandedRowCount) * (expansionFactor-1)
  }

  def partTwo(input: Array[String]): Long = ???

  @main def main(): Unit = {
    val inputLines1: Array[Array[Char]] = readInput(
      "src/main/scala/code/day11/day11.txt"
    )
    val inputLines2: Array[Array[Char]]= readInput("src/main/scala/code/day11/day11_2.txt")
    expansionFactor = 2
    println(s"Part One: ${partOne(inputLines1)}")
    expansionFactor = 1000000
    println(s"Part Two: ${partOne(inputLines2)}")
  }
}
