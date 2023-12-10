package code.day5

import scala.io.Source
import scala.util.Using
import java.util.Optional
import code.day5.Day5.readInput

object Day5 {
  def readInput(fileName: String): String =
    Using(Source.fromFile(fileName))(_.mkString).get

  def partOne(input: String): Long = {
    val chunks: List[String] = input.split("\n\n").toList
    val seeds: List[Long] =
      chunks.head.split(":")(1).trim.split(" ").map(_.toLong).toList
    val mapStrs: List[String] = chunks.tail
    var nextMap: Map[String, String] =
      mapStrs.map(parseMapping).map(_._1).map(x => x._1 -> x._2).toMap
    var maps: Map[(String, String), List[(Long, Long, Long)]] = mapStrs
      .map(parseMapping)
      .map(x => x._1 -> x._2)
      .toMap
    def getLocation(name: String, value: Long): Long = {
      var next = nextMap.get(name).get
      val mapping = maps
        .get((name, next))
        .get
        .filter(x => value >= x._2 && value <= x._2 + x._3)
      val nextValue =
        if mapping.isEmpty then value
        else mapping.map(x => x._1 + (value - x._2))(0)
      if next == "location" then nextValue
      else getLocation(next, nextValue)
    }
    seeds.map(s => getLocation("seed", s)).min
  }

  def parseMapping(
      mapChunck: String
  ): ((String, String), List[(Long, Long, Long)]) = {
    val titlePattern = """(\w+)-to-(\w+) map:""".r
    val infoPattern = """(\d+) (\d+) (\d+)""".r
    val srcDest = mapChunck.split("\n").head match
      case titlePattern(src, dest) => (src, dest)
    val destSrcLen = mapChunck
      .split("\n")
      .tail
      .map { case infoPattern(destStart, srcStart, len) =>
        (destStart.toLong, srcStart.toLong, len.toLong)
      }
      .toList

    (srcDest, destSrcLen)
  }

  def partTwo(input: String): Long = {
    val chunks: List[String] = input.split("\n\n").toList
    var nextMap: Map[String, String] =
      chunks.tail.map(parseMapping).map(_._1).map(x => x._1 -> x._2).toMap
    var maps: Map[(String, String), List[(Long, Long, Long)]] = chunks.tail
      .map(parseMapping)
      .map(x => x._1 -> x._2)
      .toMap
    val pattern = """(\d+)\s(\d+)+""".r
    def getLocation(name: String, start: Long, len: Long): Long = {
      val nextName = nextMap.get(name).get
      val mapping = maps
        .get((name, nextName))
        .get
        .filter(x => start >= x._2 && start <= x._2 + x._3)
      0
    }

    pattern
      .findAllIn(chunks(0))
      .toList
      .map { case pattern(start, len) =>
        (start.toLong, len.toInt)
      }
      .map((start, len) => getLocation("seed", start, len))
      .min
  }
  @main def main(): Unit = {
    // println(
    //   s"Part One: ${partOne(readInput("src/main/scala/code/day5/day5.txt"))}"
    // )
    println(
      s"Part Two: ${partTwo(readInput("src/main/scala/code/day5/day5_2.txt"))}"
    )
  }
}
