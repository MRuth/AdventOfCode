import scala.io.Source

/**
  * Created by monta on 6/30/2016.
  */

object Day14 extends App {
  val regexExtractor = """([A-Z]\w+)\D+(\d+)\D+(\d+)\D+(\d+)""".r
  val in = Source.fromFile("Files/Day14.in").getLines().toList

  val reindeer = in.map { str => regexExtractor.findAllMatchIn(str).next.subgroups match {
    case List(name, speed, sprint, rest) => Reindeer(name, speed.toInt, sprint.toInt, rest.toInt)
  }
  }

  def part_1(): String = {

    val reindeer = this.reindeer.map(r => r.copy())
    return reindeer.map(_.distance(2503)).max.toString
  }

  def part_2(): String = {
    val reindeer = this.reindeer.map(r => r.copy())

    for (i <- 1 to 2503) {
      reindeer.view.filter(_.distance(i) == reindeer.view.map(_.distance(i)).max).foreach(_.reward)
    }

    return (reindeer.map(_.points).max.toString)
  }

  case class Reindeer(val name: String, val speed: Int, val sprintSeconds: Int, val restingSeconds: Int) {
    private var pointsHidden = 0

    def reward = {
      pointsHidden += 1
    }

    def points = pointsHidden

    def distance(s: Int) = {
      (speed * sprintSeconds * (s / (sprintSeconds + restingSeconds))) +
        speed * Math.min(s % (sprintSeconds + restingSeconds), sprintSeconds)
    }
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}