package code.day1
import scala.io.Source
import scala.util.Using

object Day1 {
  // read each like of the input file as a String in the list
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }

  def partOne(input: List[String]): Int = {
    val numberPattern = raw"\d".r
    input
      .map(str => numberPattern.findAllIn(str).map(_.toInt).toList)
      .map(nums => nums.head * 10 + nums.last)
      .sum
  }

  def partTwo(input: List[String]): Int = {
    // val pattern = "(one|two|three|four|five|six|seven|eight|nine|\\d)".r
    val numberMap: Map[String, Int] = Map(
      "one" -> 1,
      "two" -> 2,
      "three" -> 3,
      "four" -> 4,
      "five" -> 5,
      "six" -> 6,
      "seven" -> 7,
      "eight" -> 8,
      "nine" -> 9
    )
    val numStrs = numberMap.keySet ++ "123456789".map(_.toString).toSet
    def findNums(str: String): List[Int] = {
      val strOrdering = for
        (c, i) <- str.zipWithIndex
        targetStr <- numStrs
        if (targetStr.charAt(0) == c && i + targetStr.length() <= str
          .length() && str.substring(i, i + targetStr.length()) == targetStr)
      yield (i, targetStr)
      strOrdering.sortBy(_._1).map(x => strToInt(x._2)).toList
    }
    def strToInt(str: String): Int = {
      // println(str)
      numberMap.get(str).getOrElse(str.toInt)
    }

    input.map(findNums).map(nums => nums.head * 10 + nums.last).sum
  }

  @main def main(): Unit = {
    val inputLines1: List[String] = readInput("src/main/scala/code/day1/day1.txt")
    val inputLines2: List[String] = readInput("src/main/scala/code/day1/day1_2.txt")
    println(s"Part One: ${partOne(inputLines1)}")
    println(s"Part Two: ${partTwo(inputLines2)}")
  }

}
