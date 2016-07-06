

/**
  * Created by monta on 7/6/2016.
  */

object Day20 extends App {
  val presentGoal = 36000000

  def part_1(): String = {
    (1 to presentGoal / 10).find { n =>
      (1 to Math.sqrt(n).toInt).filter(n % _ == 0)
        .flatMap(f => List(f, n / f)).map(_ * 10).sum >= presentGoal
    }.get.toString
  }

  def part_2(): String = {
    (1 to presentGoal / 10).find { n =>
      (1 to Math.sqrt(n).toInt).filter(n % _ == 0)
        .flatMap(f => List(f, n / f)).filter {
        _ * 50 >= n
      }
        .map(_ * 11).sum >= presentGoal
    }.get.toString
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}
