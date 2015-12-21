
/**
  * Created by Montana Ruth on 12/20/2015.
  */

object Day10 extends App {
  def lookAndSay(input: String, reps: Int) = {
    val matcher = """(.)\1*""".r
    (1 to reps).foldLeft(input) {(prev,c) =>
      matcher.replaceAllIn(prev, r => r.group(0).length + r.group(1))
    }
  }

  val input = "1321131112"

  def part_1(): String = {
    lookAndSay(input,40).length.toString
  }

  def part_2(): String = {
    lookAndSay(input,50).length.toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")
}
