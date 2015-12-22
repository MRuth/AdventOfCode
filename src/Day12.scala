import scala.io.Source
import scala.util.parsing.json.JSON

/**
  * Created by Montana Ruth on 12/21/2015.
  */

object Day12 extends App {

  val in = Source.fromFile("Files/Day12.in").getLines().next()
  val jsonIn = JSON.parseFull(in).get

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case i: List[_] => flatten(i)
    case e => List(e)
  }

  def part_1(): String = {
    def unpack(v : Any): Any = v match {
      case n: Double => n.toInt
      case l: List[Any] => l.map(unpack)
      case m: Map[Any,Any] => m.values.map(unpack)
      case _ => Nil
    }
    flatten(unpack(jsonIn).asInstanceOf[List[Any]]).asInstanceOf[List[Int]].sum.toString
  }

  def part_2(): String = {
    def unpack(v : Any): Any = v match {
      case n: Double => n.toInt
      case l: List[Any] => l.map(unpack)
      case x: Map[Any,Any] if(x.values.filter(_.isInstanceOf[String]).map(_.asInstanceOf[String]).exists(_ == "red")) => Nil
      case m: Map[Any,Any] => m.values.map(unpack)
      case _ => Nil
    }
    flatten(unpack(jsonIn).asInstanceOf[List[Any]]).asInstanceOf[List[Int]].sum.toString
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}
