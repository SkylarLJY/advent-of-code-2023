#!/bin/bash

# Usage: ./createAocTemplate.sh <dayNumber>

DAY=$1
DIR="src/main/scala/code/day$DAY"
FILE="Day${DAY}.scala"
INPUT="day${DAY}.txt"
INPUT2="day${DAY}_2.txt"

if [ -z "$DAY" ]; then
    echo "Please provide the day number"
    exit 1
fi

mkdir -p $DIR
touch $DIR/$INPUT
touch $DIR/$INPUT2

cat <<EOF >$DIR/$FILE
package code.day${DAY}

import scala.io.Source
import scala.util.Using

object Day${DAY} {
    def readInput(fileName: String): List[String] =
        Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
            println("error reading from input file")
            List()
        }

    def partOne(games: List[String]): Int = ???

    def partTwo(games: List[String]): Int = ???

    @main def main(): Unit = {
        val inputLines1: List[String] = readInput("src/main/scala/code/day${DAY}/day${DAY}.txt")
        val inputLines2: List[String] = readInput("src/main/scala/code/day${DAY}/day${DAY}_2.txt")
        println(s"Part One: \${partOne(inputLines1)}")
        println(s"Part Two: \${partTwo(inputLines2)}")
    }
}
EOF

echo "Created template for Day $DAY"
