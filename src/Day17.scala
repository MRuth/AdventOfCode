import scala.io.Source

/**
  * Created by monta on 7/6/2016.
  */

object Day17 extends App {
  val containers = Source.fromFile("Files/Day17.in").getLines.map(_.toInt)
  val containerSets = containers.zipWithIndex.toSet
  val containerCombos = containerSets.subsets.filter(_.toSeq.map(_._1).sum == 150).toSeq

  def part_1(): String =
    containerCombos.size.toString

  def part_2(): String =
    containerCombos.count(c => c.size == containerCombos.map(_.size).min).toString

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}
