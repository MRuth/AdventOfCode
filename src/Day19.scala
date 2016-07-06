import scala.io.Source

/**
  * Created by monta on 7/6/2016.
  */

object Day19 extends App {
  val in = Source.fromFile("Files/Day19.in").getLines()
  val inRegex = """(\w+) => (\w+)""".r
  val regex = """([A-Ze]{1}[almihnrg]*)""".r
  val replacementMap = in.takeWhile(_ != "").map { str => str.split(" => ") match {
    case Array(mol, repl) => (mol, repl)
  }
  }.toSeq.groupBy(_._1).mapValues(_.map(_._2))
  val inStr = in.next()

  def part_1(): String = {
    val molecules = regex.findAllIn(inStr).toList

    molecules.indices.flatMap { n =>
      replacementMap.get(molecules(n)).map { subList =>
        subList.map { st =>
          molecules.slice(0, n).mkString + st + molecules.slice(n + 1, molecules.size).mkString
        }
      }
    }.flatten.distinct.length.toString
  }


  println(s"Part 1: ${part_1()}")
}
