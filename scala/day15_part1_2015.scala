import scala.io.Source
import scala.util.Try

case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int)

object Main {
  def main(args: Array[String]): Unit = {
    val ingredients = readIngredients("input.txt")
    val maxScore = findMaxScore(ingredients, 100)
    println(maxScore)
  }

  def readIngredients(filename: String): List[Ingredient] = {
    val lines = Source.fromFile(filename).getLines().toList
    lines.flatMap { line =>
      val parts = line.split("\\s+")
      if (parts.length < 11) None
      else {
        val capacity = Try(parts(2).init.toInt).toOption
        val durability = Try(parts(4).init.toInt).toOption
        val flavor = Try(parts(6).init.toInt).toOption
        val texture = Try(parts(8).init.toInt).toOption
        for {
          c <- capacity
          d <- durability
          f <- flavor
          t <- texture
        } yield Ingredient(parts(0), c, d, f, t)
      }
    }
  }

  def findMaxScore(ingredients: List[Ingredient], totalTeaspoons: Int): Int = {
    calculateMaxScore(ingredients, 0, totalTeaspoons, List())
  }

  def calculateMaxScore(ingredients: List[Ingredient], index: Int, remaining: Int, teaspoons: List[Int]): Int = {
    if (index == ingredients.length - 1) {
      val newTeaspoons = teaspoons :+ remaining
      score(ingredients, newTeaspoons)
    } else {
      (0 to remaining).map { i =>
        calculateMaxScore(ingredients, index + 1, remaining - i, teaspoons :+ i)
      }.max
    }
  }

  def score(ingredients: List[Ingredient], teaspoons: List[Int]): Int = {
    var capacity = 0
    var durability = 0
    var flavor = 0
    var texture = 0
    for ((ingredient, teaspoon) <- ingredients.zip(teaspoons)) {
      capacity += ingredient.capacity * teaspoon
      durability += ingredient.durability * teaspoon
      flavor += ingredient.flavor * teaspoon
      texture += ingredient.texture * teaspoon
    }
    if (capacity < 0) capacity = 0
    if (durability < 0) durability = 0
    if (flavor < 0) flavor = 0
    if (texture < 0) texture = 0
    capacity * durability * flavor * texture
  }
}