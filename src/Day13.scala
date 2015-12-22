import scala.io.Source

/**
  * Created by Montana Ruth on 12/21/2015.
  */

object Day13 extends App {
  val happinessRegex = """(\w+) would (\w+) (\d+) happiness units by sitting next to (\w+)\.""".r
  val in = Source.fromFile("Files/Day13.in").mkString

  val happinessMap : Map[(String,String),Int] =
    happinessRegex.findAllMatchIn(in).map{ m =>
      m.group(2) match {
        case "gain" => ((m.group(1),m.group(4)) -> m.group(3).toInt)
        case "lose" => ((m.group(1),m.group(4)) -> m.group(3).toInt * -1)
      }
    }.toMap

  val guestList = happinessMap.keySet.map(_._1).toList.distinct

  def part_1(): String = {
    guestList.permutations.map{perm => (perm.sliding(2).toList :+ List(perm.head,perm.last)).map{ case List(p1,p2) =>
      happinessMap(p1,p2) + happinessMap(p2,p1)}.sum}.max.toString
  }

  def part_2(): String = {
    val guestListWithMe = guestList :+ "Me"
    guestListWithMe.permutations.map{perm => (perm.sliding(2).toList :+ List(perm.head,perm.last)).map{ case List(p1,p2) =>
      happinessMap.getOrElse((p1,p2),0) + happinessMap.getOrElse((p2,p1),0)}.sum}.max.toString
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}
