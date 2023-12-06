package code.day2
import scala.io.Source
import scala.util.Using

case class Game(
    id: Int,
    hands: List[Hand]
) // can talk about different choices like class, type
case class Hand(red: Int, green: Int, blue: Int)

object Day2 {
  def readInput(file: String): List[String] = {
    Using(Source.fromFile(file))(_.getLines().toList).getOrElse {
      println("failed to read from input file")
      List()
    }
  }

  def parseHand(hand: String): Hand = {
    val pattern = raw"(\d+)\s(red|green|blue)".r
    var (red, green, blue) = (0, 0, 0)
    pattern.findAllIn(hand).map(_.split(" ")).foreach {
      case Array(num, "red")   => red = num.toInt
      case Array(num, "green") => green = num.toInt
      case Array(num, "blue")  => blue = num.toInt
      case _                   => ("red", 0)
    }
    Hand(red, green, blue)

  }

  def parseGame(game: String): Game = {
    val id = game.split(": ")(0).split(" ")(1).toInt
    val hands: List[Hand] =
      game.split(":")(1).split(";").map(_.trim).map(parseHand).toList
    Game(id, hands)
  }

  def possibleGame(game: Game): Boolean = {
    game.hands.map(_.red).max <= 12 
      && game.hands.map(_.green).max <= 13 
      && game.hands.map(_.blue).max <= 14
  }

  def gamePower(game: Game): Int = {
    game.hands.map(_.red).max 
      * game.hands.map(_.green).max 
      * game.hands.map(_.blue).max
  }

  def partOne(games: List[String]): Int = {
    games.map(parseGame).filter(possibleGame).map(_.id).sum
  }

  def partTwo(games: List[String]): Int = {
    games.map(parseGame).map(gamePower).sum
  }

  @main def main(): Unit = {
    var partOneInput = readInput("src/main/scala/code/day2/day2.txt")
    var partTwoInput = readInput("src/main/scala/code/day2/day2_2.txt")
    println(s"part 1: ${partOne(partOneInput)}")
    println(s"part 2: ${partTwo(partTwoInput)}")
  }
}
