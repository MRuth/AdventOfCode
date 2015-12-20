import java.io.{File, FileWriter, PrintWriter}

import scala.collection._
import scala.io.Source

/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day7 extends App {
  val in = Source.fromFile("Files/Day7.in").getLines()
  new File("src/eval").mkdir()
  val out = new PrintWriter(new FileWriter("src/eval/Day7_1_eval.scala"))

  val and = """(.*) AND (.*) -> (.*)""".r
  val or = """(.*) OR (.*) -> (.*)""".r
  val not = """NOT (.*) -> (.*)""".r
  val lShift = """(.*) LSHIFT (.*) -> (.*)""".r
  val rShift = """(.*) RSHIFT (.*) -> (.*)""".r
  val assign = """(.*) -> (.*)""".r

  out.println("package eval\n")
  out.println("object Day7_1_eval extends App {")
  in.map(parseLine).foreach(str => out.println(s"  $str"))
  out.println("\n  println(aWire)\n}")
  out.close()
  def parseLine(str: String): String = {

    str match {
      case and(a, b, out) => s"lazy val ${out}Wire = (${if(a.forall(_.isDigit)) a else a + "Wire"} & ${if(b.forall(_.isDigit)) b else b + "Wire"}) & 65565"
      case or(a, b, out) => s"lazy val ${out}Wire = (${if(a.forall(_.isDigit)) a else a + "Wire"} | ${if(b.forall(_.isDigit)) b else b + "Wire"}) & 65535"
      case not(a, out) => s"lazy val ${out}Wire = 65535 & ~${if(a.forall(_.isDigit)) a else a + "Wire"}"
      case lShift(a, b, out) => s"lazy val ${out}Wire = (${if(a.forall(_.isDigit)) a else a + "Wire"} << ${if(b.forall(_.isDigit)) b else b + "Wire"}) & 65535"
      case rShift(a, b, out) => s"lazy val ${out}Wire = (${if(a.forall(_.isDigit)) a else a + "Wire"} >> ${if(b.forall(_.isDigit)) b else b + "Wire"}) & 65535"
      case assign(a, out) => s"lazy val ${out}Wire = ${if(a.forall(_.isDigit)) a else a + "Wire"} & 65535"
      case _ => ""
    }
  }


}
