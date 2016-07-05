import scala.io.Source

/**
  * Created by monta on 7/5/2016.
  */

object Day18 extends App {
  val origGrid = Source.fromFile("files/Day18.in").getLines.map { s => s.filter(_ >= ' ').toArray }.toArray
  val stuckCorners = Array((0, 0), (0, origGrid(0).length - 1),
    (origGrid.length - 1, 0), (origGrid.length - 1, origGrid(origGrid.length - 1).length - 1))

  def getNeighboringValues(grid: Array[Array[Char]], x: Int, y: Int) = List((x - 1, y - 1), (x - 1, y),
    (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1),
    (x + 1, y), (x + 1, y + 1)).flatMap(t => grid.lift(t._1).flatMap(_.lift(t._2)))

  def turnOnFourCorners(grid: Array[Array[Char]]): Array[Array[Char]] =
    stuckCorners.foldLeft(grid) { (grid, t) => grid.updated(t._1, grid(t._1).updated(t._2, '#')) }

  def simulateWithFunction(f: Array[Array[Char]] => Array[Array[Char]]) = {
    (1 to 100).view.foldLeft(f(origGrid)) { (nGrid, i) =>
      f(nGrid.indices.map { x => nGrid(x).indices.map { y =>
        (nGrid(x)(y), getNeighboringValues(nGrid, x, y).count(_ == '#')) match {
          case ('#', 2) | ('#', 3) | ('.', 3) => '#'
          case _ => '.'
        }
      }.toArray
      }.toArray)
    }.flatten.count(_ == '#').toString
  }

  def part_1(): String = {
    simulateWithFunction(f => f)
  }

  def part_2(): String = {
    simulateWithFunction(turnOnFourCorners(_))
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}