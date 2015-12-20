import scala.io.Source

/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day2 extends App {

  def part_1(): String = {
    val in = Source.fromFile("Files/Day2.in").getLines().map(_.split("x").map(_.toInt))

    val wrappingPaper = in.map{dims =>
      val Array(l,w,h) = dims
      val sArea = (2 * l * w) + (2 * w * h) + (2 * l * h)
      val extra = dims.toSeq.sorted.toList.take(2).product
      sArea + extra
    }

    wrappingPaper.sum.toString
  }

  def part_2(): String = {
    val in = Source.fromFile("Files/Day2.in").getLines().map(_.split("x").map(_.toInt))

    val ribbons = in.map{ dims =>
      val perimeter = dims.toSeq.sorted.take(2).map(_*2).sum
      val bow = dims.product
      perimeter + bow
    }
    ribbons.sum.toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")
}
