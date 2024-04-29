import scala.io.Source
import scala.util.Try

case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val ingredients = readIngredients("input.txt")
    val maxScore = findMaxScore(ingredients, 100, 500)
    println(maxScore)
  }

  def readIngredients(filename: String): Array[Ingredient] = {
    val lines = Source.fromFile(filename).getLines().toArray
    lines.map { line =>
      val parts = line.split("\\s+")
      Ingredient(parts(0), parts(2).init.toInt, parts(4).init.toInt, parts(6).init.toInt, parts(8).init.toInt, parts(10).toInt)
    }
  }

  def findMaxScore(ingredients: Array[Ingredient], totalTeaspoons: Int, targetCalories: Int): Int = {
    calculateMaxScore(ingredients, 0, totalTeaspoons, Array[Int](), targetCalories)
  }

  def calculateMaxScore(ingredients: Array[Ingredient], index: Int, remaining: Int, teaspoons: Array[Int], targetCalories: Int): Int = {
    if (index == ingredients.length - 1) {
      val newTeaspoons = teaspoons :+ remaining
      if (calculateCalories(ingredients, newTeaspoons) == targetCalories) {
        score(ingredients, newTeaspoons)
      } else {
        0
      }
    } else {
      var maxScore = 0
      for (i <- 0 to remaining) {
        val score = calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons :+ i, targetCalories)
        if (score > maxScore) {
          maxScore = score
        }
      }
      maxScore
    }
  }

  def score(ingredients: Array[Ingredient], teaspoons: Array[Int]): Int = {
    var capacity = 0
    var durability = 0
    var flavor = 0
    var texture = 0
    for (i <- ingredients.indices) {
      capacity += ingredients(i).capacity * teaspoons(i)
      durability += ingredients(i).durability * teaspoons(i)
      flavor += ingredients(i).flavor * teaspoons(i)
      texture += ingredients(i).texture * teaspoons(i)
    }
    if (capacity < 0) capacity = 0
    if (durability < 0) durability = 0
    if (flavor < 0) flavor = 0
    if (texture < 0) texture = 0
    capacity * durability * flavor * texture
  }

  def calculateCalories(ingredients: Array[Ingredient], teaspoons: Array[Int]): Int = {
    var calories = 0
    for (i <- ingredients.indices) {
      calories += ingredients(i).calories * teaspoons(i)
    }
    calories
  }
}