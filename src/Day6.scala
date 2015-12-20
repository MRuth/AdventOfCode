import scala.io.Source

/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day6 extends App {
  def part_1(): String = {
    val orderedPairPattern = """\d+,\d+""".r
    val in = Source.fromFile("Files/Day6.in").getLines
    val lights = Array.fill(1000)(Array.fill(1000)(false))

    def changeLights(str: String) = {
      val ins = str.takeWhile(!_.isDigit).trim
      val List(x1, y1, x2, y2) = orderedPairPattern.findAllIn(str).flatMap(s => s.split(",").map(_.toInt)).toList

      ins match {
        case "turn on" => for(x <- x1 to x2; y <- y1 to y2) lights(x)(y) = true
        case "turn off" => for(x <- x1 to x2; y <- y1 to y2) lights(x)(y) = false
        case "toggle" => for(x <- x1 to x2; y <- y1 to y2) lights(x)(y) = !lights(x)(y)
      }
    }

    in.foreach(changeLights)
    lights.flatten.count(_ == true).toString
  }

  def part_2(): String = {
    val orderedPairPattern = """\d+,\d+""".r
    val in = Source.fromFile("Files/Day6.in").getLines
    val lights = Array.fill(1000)(Array.fill(1000)(0))

    def changeLights(str: String) = {
      val ins = str.takeWhile(!_.isDigit).trim
      val List(x1, y1, x2, y2) = orderedPairPattern.findAllIn(str).flatMap(s => s.split(",").map(_.toInt)).toList

      ins match {
        case "turn on" => for(x <- x1 to x2; y <- y1 to y2) lights(x)(y) += 1
        case "turn off" => for(x <- x1 to x2; y <- y1 to y2) if(lights(x)(y) > 0) lights(x)(y) -= 1;
        case "toggle" => for(x <- x1 to x2; y <- y1 to y2) lights(x)(y) += 2
      }
    }

    in.foreach(changeLights)
    lights.flatten.sum.toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")
}
