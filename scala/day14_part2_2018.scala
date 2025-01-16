
import scala.io.Source

object ChocolateCharts {

  def solvePart1(input: Int): String = {
    val scores = Array(3, 7)
    var elf1 = 0
    var elf2 = 1
    var recipes = scores.toBuffer

    while (recipes.length < input + 10) {
      val newRecipes = (recipes(elf1) + recipes(elf2)).toString.map(_.asDigit)
      recipes ++= newRecipes
      elf1 = (elf1 + 1 + recipes(elf1)) % recipes.length
      elf2 = (elf2 + 1 + recipes(elf2)) % recipes.length
    }

    recipes.slice(input, input + 10).mkString
  }

  def solvePart2(input: String): Int = {
    val scores = Array(3, 7)
    var elf1 = 0
    var elf2 = 1
    var recipes = scores.toBuffer

    while (true) {
      val newRecipes = (recipes(elf1) + recipes(elf2)).toString.map(_.asDigit)
      for (newRecipe <- newRecipes) {
        recipes += newRecipe
        if (recipes.length >= input.length && recipes.slice(recipes.length - input.length, recipes.length).mkString == input) {
          return recipes.length - input.length
        }
      }
      elf1 = (elf1 + 1 + recipes(elf1)) % recipes.length
      elf2 = (elf2 + 1 + recipes(elf2)) % recipes.length
    }
    -1 // Should not reach here
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").getLines().next()
    println(s"Part 1: ${solvePart1(input.toInt)}")
    println(s"Part 2: ${solvePart2(input)}")
  }
}
