import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
  * Created by Montana Ruth on 12/19/2015.
  */

object Day7_alt extends App {
  val in = Source.fromFile("Files/Day7.in").getLines().toList

  var wires: mutable.HashMap[String,Int] = new mutable.HashMap()

  val and = """(.*) AND (.*) -> (.*)""".r
  val or = """(.*) OR (.*) -> (.*)""".r
  val not = """NOT (.*) -> (.*)""".r
  val lShift = """(.*) LSHIFT (.*) -> (.*)""".r
  val rShift = """(.*) RSHIFT (.*) -> (.*)""".r
  val assign = """(.*) -> (.*)""".r
  val outPat = """.* -> (.*)""".r

  val andOp = 1
  val orOp = 2
  val notOp = 3
  val lShiftOp = 4
  val rShiftOp = 5
  val assignOp = 6

  def evalParams(a: Option[String],b : Option[String]): (Option[Int],Option[Int]) = {
    (a,b) match {
      case (Some(a),Some(b)) => {
        (if(a.forall(_.isDigit)) Some(a.toInt) else wires.get(a),if(b.forall(_.isDigit)) Some(b.toInt) else wires.get(b))
      }
      case (Some(a),None) => (if(a.forall(_.isDigit)) Some(a.toInt) else wires.get(a),None)
      case (None,Some(b)) => (None,if(b.forall(_.isDigit)) Some(b.toInt) else wires.get(b))
      case _ => (None,None)
    }
  }


  def evalOperation(operation: String): Unit = {

    val (op,a,b,out) = operation match {
      case and(x,y,o) => (andOp,Some(x),Some(y),o)
      case or(x,y,o) => (orOp,Some(x),Some(y),o)
      case not(x,o) => (notOp,Some(x),None,o)
      case lShift(x,y,o) => (lShiftOp,Some(x),Some(y),o)
      case rShift(x,y,o) => (rShiftOp,Some(x),Some(y),o)
      case assign(x,o) => (assignOp,Some(x),None,o)
    }

    val (aParam,bParam) = evalParams(a,b)
    (op, aParam,bParam) match {
      case (opCode,Some(a : Int),Some(b : Int)) if(opCode == andOp) => wires(out) = (a & b) & 65535
      case (opCode,Some(a: Int),Some(b: Int)) if(opCode == orOp) => wires(out) = (a | b) & 65535
      case (opCode,Some(a: Int),_) if(opCode == notOp) => wires(out) = ~a & 65535
      case (opCode,Some(a: Int),Some(b: Int)) if(opCode == lShiftOp) => wires(out) = (a << b) & 65535
      case(opCode,Some(a: Int),Some(b: Int)) if(opCode == rShiftOp) => wires(out) = (a >> b) & 65535
      case (opCode,Some(a: Int),_) if(opCode == assignOp) => wires(out) = a & 65535

      case (opCode,None,_) if(opCode == andOp) => {evalOperation(findOperation(a.get));evalOperation(operation)}
      case (opCode,_,None) if(opCode == andOp) => {evalOperation(findOperation(b.get));;evalOperation(operation)}

      case (opCode,None,_) if(opCode == orOp) => {evalOperation(findOperation(a.get));evalOperation(operation)}
      case (opCode,_,None) if(opCode == orOp) => {evalOperation(findOperation(b.get));evalOperation(operation)}

      case (opCode,None,_) if(opCode == notOp) => {evalOperation(findOperation(a.get));evalOperation(operation)}

      case (opCode,None,_) if(opCode == lShiftOp) => {evalOperation(findOperation(a.get));evalOperation(operation)}
      case (opCode,_,None) if(opCode == lShiftOp) => {evalOperation(findOperation(b.get));evalOperation(operation)}

      case (opCode,None,_) if(opCode == rShiftOp) => {evalOperation(findOperation(a.get));evalOperation(operation)}
      case (opCode,_,None) if(opCode == rShiftOp) => {evalOperation(findOperation(b.get));evalOperation(operation)}

      case (opCode,None,_) if(opCode == assignOp) => {evalOperation(findOperation(a.get));evalOperation(operation)}

    }
  }

  def findOperation(out: String): String = {
    val findPat = s"(.*) -> $out"
    in.find(_.matches(findPat)).getOrElse("")
  }

  def part1(): Int = {
    in.foreach{str =>
      val outPat(key) = str
      if(!wires.contains(key))
        evalOperation(str)
    }
    wires("a")
  }

  def part2(b: Int): Int = {
    wires = new mutable.HashMap()
    wires("b") = b
    part1()
  }

  val part1Out = part1()
  val part2Out = part2(part1Out)

  println(s"Part 1: $part1Out\nPart 2: $part2Out")

}
