package code.day4

import scala.io.Source
import scala.util.Using

object Day4 {
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }

  def partOne(input: List[String]): Int = {
    input.map(playGame).sum
  }

  case class Game(id: Int, winningNums: List[Int], haveNums: List[Int])

  def playGame(gameStr: String): Int = {
    val game = parseGame(gameStr)
    val points = game.winningNums.count(n => game.haveNums.contains(n))
    math.pow(2, points - 1).toInt
  }

  def parseGame(game: String): Game = {
    val pattern = """Card(\s+\d+): ([\d\s]+)\|([\d\s]+)""".r
    def toListOfInt(numSeq: String): List[Int] =
      numSeq.trim.split("\\s+").map(_.toInt).toList
    game match
      case pattern(id, n1, n2) =>
        Game(id.trim.toInt, toListOfInt(n1), toListOfInt(n2))
  }

  def partTwo(input: List[String]): Int = {
    // id, res
    // fold on carry over cards?
    // id=1 res=3, 1 card, [1,1,1]
    // id=2 res=4, 1+1 cards, [2,2,1,1]
    // id=3 res=1, 1+2 cards, [3,1,1]
    input.map(parseGame).foldLeft((List(), 0))(playGamePartTwo)._2
  }
  
  def playGamePartTwo(acc: (List[Int], Int), game: Game): (List[Int], Int) = {
    val gameRes = game.winningNums.count(n => game.haveNums.contains(n))
    val currCardCount = 1+acc._1.headOption.getOrElse(0)
    (List.fill(gameRes)(currCardCount).zipAll(if acc._1.isEmpty then List() else acc._1.tail, 0, 0).map((a, b)=>a+b), acc._2+currCardCount)
  }

  @main def main(): Unit = {
    // val inputLines1: List[String] = readInput("src/main/scala/code/day4/day4.txt")
    val inputLines2: List[String] = readInput(
      "src/main/scala/code/day4/day4_2.txt"
    )
    // println(s"Part One: ${partOne(inputLines1)}")
    println(s"Part Two: ${partTwo(inputLines2)}")
  }
}
