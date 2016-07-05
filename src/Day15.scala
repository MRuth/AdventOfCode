import scala.io.Source

/**
  * Created by monta on 7/1/2016.
  */

object Day15 extends App {

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int,
                        texture: Int, calories: Int) {
    def getIngredientScore(amt: Int) = Seq(capacity * amt, durability * amt,
      flavor * amt, texture * amt, calories * amt)
  }

  val in = Source.fromFile("Files/Day15.in").getLines().toList
  val pattern = """(\w+):\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)\D+?(-?\d+)""".r

  val ingredients = in.map { str =>
    val pattern(nme, cap, dur, flv, tex, cal) = str
    Ingredient(nme, cap.toInt, dur.toInt, flv.toInt, tex.toInt, cal.toInt)
  }

  val perms = List.fill(ingredients.size)(0 to 100).
    flatten.combinations(ingredients.size).filter(_.sum == 100).
    flatMap(_.permutations).map(p => ingredients.zip(p)).toList

  def getRecipeScore(ingredientScores: Seq[Seq[Int]]) = {
    ingredientScores.transpose.dropRight(1).map(_.sum.max(0)).product
  }

  def getRecipeScoreWithCalories(ingredientScores: Seq[Seq[Int]]) = {
    val pairs = ingredientScores.transpose
    val results = (pairs.dropRight(1).map(_.sum.max(0)).product, pairs.takeRight(1).flatten.sum)
    results
  }

  def part_1(): String = {
    perms.map(p => getRecipeScore(p.map(o => o._1.getIngredientScore(o._2)))).
      max.toString
  }

  def part_2(): String = {
    perms.map(p => getRecipeScoreWithCalories(p.map(o => o._1.getIngredientScore(o._2))))
      .filter(_._2 == 500).maxBy(_._1)._1.toString
  }

  println(s"Part 1: ${part_1()}")
  println(s"Part 2: ${part_2()}")
}
