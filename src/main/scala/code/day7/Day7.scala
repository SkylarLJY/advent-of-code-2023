package code.day7

import scala.io.Source
import scala.util.Using

object Day7 {
  def readInput(fileName: String): List[String] =
    Using(Source.fromFile(fileName))(_.getLines().toList).getOrElse {
      println("error reading from input file")
      List()
    }

  val cardByStrength = "AKQT98765432J".reverse.zipWithIndex.toMap
  case class Hand(cards: String, bid: Int, handType: HandType)
  val charOrdering: Ordering[Char] =
    Ordering.by(c => cardByStrength.get(c))
  val strOrdering: Ordering[String] = (s1, s2) =>
    (for
      (c1, c2) <- s1.zip(s2)
      if c1 != c2
    yield charOrdering.compare(c1, c2))(0)
  implicit val handOrdering: Ordering[Hand] = (a: Hand, b: Hand) =>
    if a.handType.ordinal != b.handType.ordinal then
      a.handType.ordinal.compare(b.handType.ordinal)
    else strOrdering.compare(a.cards, b.cards)

  enum HandType {
    case HighCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind,
      FiveOfAKind
  }

  def parseHand(hand: String): Hand = {
    val cards = hand.split("\\s+")(0)
    val bid = hand.split("\\s+")(1).toInt
    Hand(cards, bid, getHandTypeWithJ(cards))
  }

  def getHandTypeWithJ(cards: String): HandType = {
    val countJ = cards.filter(_ == 'J').length()
    if countJ == 5 then return HandType.FiveOfAKind
    val notJCardsCount = cards
      .filter(_ != 'J')
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toList
      .sortBy(_._2)(Ordering[Int].reverse)
    notJCardsCount.head._2 + countJ match
        case 5 => HandType.FiveOfAKind
        case 4 => HandType.FourOfAKind
        case 3 if notJCardsCount.length == 2 => HandType.FullHouse
        case 3 if notJCardsCount.length == 3 => HandType.ThreeOfAKind
        case 2 if notJCardsCount.length == 3 => HandType.TwoPairs
        case 2 if notJCardsCount.length == 4 => HandType.OnePair
        case _ => HandType.HighCard
    
  }
  def partOne(input: List[String]): Int =
    input
      .map(parseHand)
      .sortBy(identity)
      .zipWithIndex
      .map((hand, idx) => hand.bid * (idx + 1))
      .sum

  def partTwo(input: List[String]): Int =
    partOne(input)

  @main def main(): Unit = {
    val inputLines1: List[String] = readInput(
      "src/main/scala/code/day7/day7.txt"
    )
    val inputLines2: List[String] = readInput("src/main/scala/code/day7/day7_2.txt")
    // println(s"Part One: ${partOne(inputLines1)}")
    println(s"Part Two: ${partTwo(inputLines2)}")
  }
}
