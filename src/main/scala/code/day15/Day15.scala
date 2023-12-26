package code.day15

import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap

object Day15 {
    def readInput(fileName: String): List[String] =
        Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
            println("error reading from input file")
            List()
        }

    def partOne(input: List[String]): Int = {
        input.flatMap(_.split(",")).map(hash).sum
    } 

    def hash(str: String): Int = {
        str.map(_.toInt).foldLeft(0)((acc, value) => (acc+value)*17 % 256)
    }

    type Lens = (String, Int)
    type Box = LinkedHashMap[String, Int]
    def partTwo(input: List[String]): Int = {
        var boxes: Array[Box] = Array.fill(256)(LinkedHashMap.empty)
        
        def putLens(ins: String): Unit = {
            
            val lens = ins.split("=") match {
                case Array(s, v) => (s, v.toInt)
            } 
            val boxNum = hash(lens._1) 
            boxes(boxNum).update(lens._1, lens._2)  
        }
        def removeLens(ins: String): Unit = {
            val lable = ins.split("-")(0)
            val boxNum = hash(lable) 
            if boxes(boxNum).contains(lable) then boxes(boxNum).remove(lable)

        }

        for ins <- input.flatMap(_.split(",")) do
            if ins.contains('=') then putLens(ins)
            else removeLens(ins)

        boxes.zipWithIndex.filter(_._1.nonEmpty).map(boxPower).sum
    }

    def boxPower(box: Box, index: Int): Int = {
        if box.isEmpty then 0
        else box.zipWithIndex.map((box, boxIndex) => box._2 * (index+1) * (boxIndex+1)).sum
    }

    @main def main(): Unit = {
        val inputLines1: List[String] = readInput("src/main/scala/code/day15/day15.txt")
        val inputLines2: List[String] = readInput("src/main/scala/code/day15/day15_2.txt")
        println(s"Part One: ${partOne(inputLines1)}")
        println(s"Part Two: ${partTwo(inputLines2)}")
    }
}
