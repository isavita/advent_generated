
import scala.io.Source
import scala.util.Random

object Solution {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.trim
    println(solve(input))
  }

  def parseInput(input: String): (Map[String, List[String]], List[String]) = {
    val blocks = input.split("\n\n")
    val startingMaterial = splitMolecules(blocks(1))
    val graph = blocks(0).split("\n").map { l =>
      val parts = l.split(" => ")
      (parts(0), parts(1))
    }.groupBy(_._1).map { case (k, v) => (k, v.map(_._2).toList) }
    (graph, startingMaterial.toList)
  }

  def splitMolecules(input: String): Array[String] = {
    input.toCharArray.foldLeft(List.empty[String]) {
      case (acc, char) if char.isUpper => acc :+ char.toString
      case (acc, char) => acc.init :+ (acc.last + char)
    }.toArray
  }

  def solve(input: String): Int = {
    val (reverseGraph, startingMols) = parseInput(input)
    val productToReactant = reverseGraph.values.flatten.map(p => (p, reverseGraph.find(_._2.contains(p)).get._1)).toMap
    var allProducts = productToReactant.keys.toArray
    var start = startingMols.mkString
    var mol = start
    var steps = 0
    val rn = new Random()

    while (mol != "e") {
      var changeMade = false
      for (prod <- allProducts) {
        val count = mol.split(prod, -1).length - 1
        if (count > 0) {
          changeMade = true
          steps += count
          mol = mol.replace(prod, productToReactant(prod))
          
        }
      }
      if (!changeMade) {
        allProducts = rn.shuffle(allProducts.toSeq).toArray
        mol = start
        steps = 0
      }
    }
    steps
  }
}
