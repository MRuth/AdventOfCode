import scala.annotation.tailrec

/**
  * Created by Montana Ruth on 12/21/2015.
  */

object Day11 extends App {
  val stringIncrement: String => String = (prev: String) => prev.last match {
    case 'z' if(prev.length > 1) => stringIncrement(prev.dropRight(1)) + "a"
    case 'z' => "aa"
    case c => prev.dropRight(1) + (c+1).toChar
  }

  val nextPassword: String => String = (input: String) => Iterator.iterate(input)(stringIncrement).find(isGoodPassword).get

  def isGoodPassword(password: String): Boolean = {
    val illegalCharsRegex = """[iol]""".r
    val pairedLettersRegex = """(.)\1""".r
    val pairedLettersInPswd = pairedLettersRegex.findAllMatchIn(password).map(_.group(0)).toList
    if(password.size < 8)
      return false
    if(illegalCharsRegex.findFirstIn(password).nonEmpty)
      return false
    if(pairedLettersInPswd.distinct.size < 2)
      return false
    if(!password.toCharArray.sliding(3).exists{case Array(a,b,c) => (a+1 == b) && (b+1 == c)})
      return false
    return true
  }

  def part_1(): String = {
    val part1Input = "cqjxjnds"
    nextPassword(part1Input)
  }

  def part_2(): String = {
    nextPassword(stringIncrement(part_1()))
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")

}
