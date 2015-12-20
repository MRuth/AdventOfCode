import scala.io.Source

/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day5 extends App {

  def part_1(): String = {
    val vowels = ".*[aeiou]+.*[aeiou]+.*[aeiou]+.*"
    val repeat = """.*(.)\1.*"""
    val notAllowed = """.*(ab|cd|pq|xy).*"""

    val in = Source.fromFile("Files/Day5.in").getLines()

    def isNiceWord(str: String): Boolean = {
      !str.matches(notAllowed) && str.matches(vowels) && str.matches(repeat)
    }

    in.count(isNiceWord).toString
  }

  def part_2(): String = {
    val in = Source.fromFile("Files/Day5.in").getLines()

    val twoPairs =  """.*(..).*\1.*"""
    val repeat = """.*(.).\1.*"""

    def isNiceWord(str: String): Boolean = {
      str.matches(twoPairs) && str.matches(repeat)
    }

    in.count(isNiceWord).toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")

}
