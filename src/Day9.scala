import scala.collection.mutable
import scala.io.Source

/**
  * Created by Montana Ruth on 12/20/2015.
  */

object Day9 extends App {

  val in = Source.fromFile("Files/Day9.in").getLines().toList
  val routes: mutable.HashMap[(String,String),Int] = new mutable.HashMap()

  val mappedRoutes = in.map(s => route(s))

  mappedRoutes.foreach{ r =>
    routes.getOrElseUpdate((r.to,r.from),r.dist)
    routes.getOrElseUpdate((r.from,r.to),r.dist)
  }

  val routeList = mappedRoutes.flatMap(r => List(r.to,r.from)).distinct
  val routePerms = routeList.permutations

  val travelDistances = routePerms.map { rList =>
    rList.sliding(2).map{case List(from,to) => (from,to)}.map(routes(_)).sum
  }.toList


  def part_1(): String = {
    travelDistances.min.toString
  }

  def part_2(): String = {
    travelDistances.max.toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")
}

case class route(from: String, to: String, dist: Int)

object route {
  private val regexString = """(\w+) to (\w+) = (\d+)""".r
  def apply(str: String) = {val regexString(from,to,dist) = str; new route(from,to,dist.toInt)}
}