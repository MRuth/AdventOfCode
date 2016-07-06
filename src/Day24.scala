import scala.io.Source

/**
  * Created by monta on 7/6/2016.
  */

object Day24 extends App {
  val in = Source.fromFile("Files/Day24.in").getLines.map(_.toLong).toList.sorted
  val inSum = in.sum

  def packagesNeededPerContainer(numContainers: Int) = {
    val list = in.reverse.scanLeft(0L)(_ + _).takeWhile(_ <= inSum / numContainers)
    if (list.max < inSum / numContainers)
      list.length + 1
    else
      list.length
  }

  def getMinQuantumScore(numContainers: Int) = {
    val combinationSize = packagesNeededPerContainer(numContainers)
    in.combinations(combinationSize).filter(_.sum == inSum / numContainers)
      .map(_.product).min
  }

  def part_1(): String = getMinQuantumScore(3).toString

  def part_2(): String = getMinQuantumScore(4).toString

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}
