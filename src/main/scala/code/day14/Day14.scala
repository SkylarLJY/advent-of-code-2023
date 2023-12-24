package code.day14

import scala.io.Source
import scala.util.Using

object Day14 {
    def readInput(fileName: String): List[String] =
        Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
            println("error reading from input file")
            List()
        }

    def partOne(input: List[String]): Int = 
        input.transpose.map(charList => tiltLeft(charList.toArray)).map(calculateWeight).sum

    def tiltLeft(rocks: Array[Char]): Array[Char] = {
        // tilt to the left after transposing
        val rounded = rocks.zipWithIndex.filter(_._1 == 'O').map(_._2)
        var prev = 0
        // two pointers: the next untilted O and the next position to put it
        for r <- rounded do
            if r==0 || "O#".contains(rocks(r-1)) then prev = r+1
            else 
                if rocks.slice(prev, r).contains('#') then 
                    prev = rocks.slice(prev, r).zipWithIndex.findLast(_._1=='#').map(_._2).get + prev + 1
                assert(rocks(prev) == '.')
                rocks(prev) = 'O'
                rocks(r) = '.'
                prev += 1
        
        // calculateWeight(rocks)
        rocks
    }

    def calculateWeight(rocks: Array[Char]): Int = 
        rocks.zipWithIndex.filter(_._1 == 'O').map(rocks.length - _._2).sum

    def partTwo(input: List[String]): Int = 
        var inputArr = input.map(_.toArray).toArray
        for i <- 0 until 1000000000 do 
            // north
            inputArr = inputArr.transpose.map(tiltLeft).transpose
            // west 
            inputArr = inputArr.map(tiltLeft)     
            // south
            inputArr = inputArr.transpose.map(_.reverse).map(tiltLeft).map(_.reverse).transpose
            // east
            inputArr = inputArr.map(_.reverse).map(tiltLeft).map(_.reverse)
            // inputArr.foreach(x=>println(x.mkString))
            // println("====================")

        inputArr.transpose.map(calculateWeight).sum

    @main def main(): Unit = {
        // val inputLines1: List[String] = readInput("src/main/scala/code/day14/day14.txt")
        val inputLines2: List[String] = readInput("src/main/scala/code/day14/day14_2.txt")
        // println(s"Part One: ${partOne(inputLines1)}")
        println(s"Part Two: ${partTwo(inputLines2)}")
    }
}
