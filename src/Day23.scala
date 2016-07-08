import scala.collection.mutable
import scala.io.Source

/**
  * Created by monta on 7/8/2016.
  */

object Day23 extends App {
  val in = Source.fromFile("Files/Day23.in").getLines().map(_.replaceAll(",", "").split(" ")).toArray

  def evalProgram(registers: mutable.Map[String, Long]): String = {
    var curr: Array[String] = null
    var itr = 0
    while (itr < in.length) {
      if (registers("a") < 0)
        println()
      curr = in(itr)
      curr match {
        case Array("hlf", r) => registers(r) /= 2; itr += 1
        case Array("tpl", r) => registers(r) *= 3; itr += 1
        case Array("inc", r) => registers(r) += 1; itr += 1
        case Array("jmp", o) => itr += o.toInt
        case Array("jie", r, o) => itr += (if (registers(r) % 2 == 0) o.toInt else 1)
        case Array("jio", r, o) => itr += (if (registers(r) == 1) o.toInt else 1)
      }

    }

    registers("b").toString
  }

  def part_1(): String =
    evalProgram(mutable.Map("a" -> 0, "b" -> 0))

  def part_2(): String =
    evalProgram(mutable.Map("a" -> 1, "b" -> 0))

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")

}
