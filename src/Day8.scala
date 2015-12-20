import scala.io.Source

/**
  * Created by Montana Ruth on 12/19/2015.
  */

object Day8 extends App {
  def part_1(): String = {
    val in = Source.fromFile("Files/Day8.in").getLines().toList
    val specialCharRegex = """(\\[\\"])|(\\x(..)(?!(\\\\[\\"])))""".r

    val parsedIn = in.map { s =>
      specialCharRegex.replaceAllIn(s.substring(1,s.length-1),"+").length
    }

    val numCharsInCode = in.map(_.replaceAll( """\s""", "")).map(_.length).sum
    val numCharsParsed = parsedIn.sum

    (numCharsInCode - numCharsParsed).toString
  }

  def part_2(): String = {
    val in = Source.fromFile("Files/Day8.in").getLines().toList
    val regexPatternMatch = """([\\"])([^\\"]*)""".r

    val encoded = in.map{s =>
      val matcher = regexPatternMatch.findAllMatchIn(s).toList
      val special = matcher.map(_.group(1)).map("\\" + _)
      val regular = matcher.map(_.group(2))

      special.zip(regular).map{
        case (s,r) => s+r
      }.mkString("\"","","\"")
    }

    val numCharsInSource=in.map(_.length).sum
    val numCharsInEncoded = encoded.map(_.length).sum

    (numCharsInEncoded-numCharsInSource).toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")
}

