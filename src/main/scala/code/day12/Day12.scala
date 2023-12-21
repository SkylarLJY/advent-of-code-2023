package code.day12

import scala.io.Source
import scala.util.Using
import scala.compiletime.ops.string
import scala.collection.mutable

object Day12 {
    def readInput(fileName: String): List[String] =
        Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
            println("error reading from input file")
            List()
        }

    def partOne(input: List[String]): Long = 
        input.map(parseInputLine).map(getArrangementCount).sum

    def parseInputLine(line: String): (String, List[Int]) = {
        (line.split(" ")(0), line.split(" ")(1).split(",").map(_.toInt).toList)
    }

    val cache = mutable.Map.empty[(String, List[Int]), Long]

    def getArrangementCount(condition: String, rules: List[Int]): Long = {
        cache.get((condition, rules)) match
            case Some(value) => value
            case None => 
                if rules.isEmpty && condition.contains('#') then return 0
                if rules.isEmpty && !condition.contains('#') then return 1
                if condition.isBlank && !rules.isEmpty then return 0
                if condition.isBlank && rules.isEmpty then return 1

                var res = 0L
                if condition(0) == '?' || condition(0) == '.' then
                    res += getArrangementCount(condition.substring(1), rules)
                if condition(0) == '?' || condition(0) == '#' then 
                    if  condition.length >= rules(0) && !condition.substring(0, rules(0)).contains('.') && 
                            (condition.length()==rules(0) || condition.charAt(rules(0)) != '#') then
                        if condition.length() == rules(0) then res += getArrangementCount("", rules.tail)
                        else res += getArrangementCount(condition.substring(rules(0)+1), rules.tail) 
                
                cache((condition, rules)) = res
                res            
    }

    def partTwo(input: List[String]): Long = 
        input.map(parseInputLine).map((springs, nums) => (List.fill(5)(springs).mkString("?"), List.fill(5)(nums).flatten)).map(getArrangementCount).sum

    @main def main(): Unit = {
        val inputLines1: List[String] = readInput("src/main/scala/code/day12/day12.txt")
        println(s"Part One: ${partOne(inputLines1)}")
        println(s"Part Two: ${partTwo(inputLines1)}")
    }
}
