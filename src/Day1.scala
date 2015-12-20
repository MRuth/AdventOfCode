import java.io.File

import scala.io.Source

/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day1 extends App {


  def part_1(): String = {
    val in = Source.fromFile("Files/Day1.in").map{
      case '(' => 1
      case ')' => -1
    }

    in.foldLeft(0){_ + _}.toString
  }

  def part_2(): String = {
    val in = Source.fromFile("Files/Day1.in")
    val mapped = in.map{
      case '(' => 1
      case ')' => -1
    }

    mapped.scanLeft(0){_ + _}.takeWhile(_ > -1).size.toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")
}
