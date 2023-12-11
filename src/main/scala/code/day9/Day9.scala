package code.day9

import scala.io.Source
import scala.util.Using

object Day9 {
    def readInput(fileName: String): List[String] =
        Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
            println("error reading from input file")
            List()
        }

    def partOne(input: List[String]): Int = 
        input.map(_.split(" ").map(_.toInt).toList).map(getPrediction).sum
       

    def getPrediction(input: List[Int]): Int = 
        var lastNum: List[Int] = List(input.last)
        var nums = input
        while !nums.forall(_==0) do 
            nums = nums.zip(nums.tail).map((a, b) => b-a)
            lastNum = nums.last :: lastNum
        lastNum.foldLeft(0)(_+_)

    def partTwo(input: List[String]): Int = 
        input.map(_.split(" ").map(_.toInt).toList).map(getPrev).sum

    def getPrev(input: List[Int]): Int = 
        var firstNum = List(input.head)
        var nums = input
        while !nums.forall(_==0) do 
            nums = nums.zip(nums.tail).map((a, b) => b-a)
            firstNum = nums.head :: firstNum
        firstNum.foldLeft(0)(-_+_)

    @main def main(): Unit = {
        val inputLines1: List[String] = readInput("src/main/scala/code/day9/day9.txt")
        val inputLines2: List[String] = readInput("src/main/scala/code/day9/day9_2.txt")
        // println(s"Part One: ${partOne(inputLines1)}")
        println(s"Part Two: ${partTwo(inputLines2)}")
    }
}
