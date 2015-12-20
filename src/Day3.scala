import scala.io.Source

/**
  * Created by Montana Ruth on 12/18/2015.
  */

object Day3 extends App {

  def getHouseLoc(prevLoc: (Int,Int), direction: Char): (Int,Int) = {
    val (x,y) = prevLoc
    direction match {
      case '>' => (x+1,y)
      case '<' => (x-1,y)
      case '^' => (x,y+1)
      case 'v' => (x,y-1)
    }
  }

  def part_1(): String = {
    val in = Source.fromFile("Files/Day3.in")

    val houseLocations = in.scanLeft((0,0)){ (prev,dir) =>
      getHouseLoc(prev,dir)
    }

    houseLocations.toList.distinct.size.toString
  }

  def part_2(): String = {
    val in = Source.fromFile("Files/Day3.in").toArray

    val santaHouses = (for(i <- 0 until in.size by 2) yield in(i)).scanLeft((0,0)){(prev,dir) =>
      getHouseLoc(prev,dir)
    }.toList

    val roboSantaHouses = (for(i <- 1 until in.size by 2) yield in(i)).scanLeft((0,0)){(prev,dir) =>
      getHouseLoc(prev,dir)
    }.toList

    val visitedHouses = santaHouses ::: roboSantaHouses

    visitedHouses.distinct.size.toString
  }

  println(s"Part 1: ${part_1}")
  println(s"Part 2: ${part_2}")

}
