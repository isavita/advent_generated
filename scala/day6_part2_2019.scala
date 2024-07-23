
import scala.io.Source
import scala.collection.mutable

object UniversalOrbitMap {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val orbitMap = buildOrbitMap(lines)

    // Part 1: Calculate total orbits
    val totalOrbits = calculateTotalOrbits(orbitMap)
    println(s"Total number of direct and indirect orbits: $totalOrbits")

    // Part 2: Calculate minimum transfers from YOU to SAN
    val transfers = calculateOrbitalTransfers(orbitMap, "YOU", "SAN")
    println(s"Minimum number of orbital transfers required: $transfers")
  }

  def buildOrbitMap(lines: List[String]): Map[String, String] = {
    lines.map { line =>
      val parts = line.split("\\)")
      parts(1) -> parts(0) // (child -> parent)
    }.toMap
  }

  def calculateTotalOrbits(orbitMap: Map[String, String]): Int = {
    orbitMap.keys.map(countOrbits(orbitMap, _)).sum
  }

  def countOrbits(orbitMap: Map[String, String], objectName: String): Int = {
    orbitMap.get(objectName) match {
      case Some(parent) => 1 + countOrbits(orbitMap, parent)
      case None => 0
    }
  }

  def calculateOrbitalTransfers(orbitMap: Map[String, String], start: String, end: String): Int = {
    val startPath = findPathToCOM(orbitMap, start)
    val endPath = findPathToCOM(orbitMap, end)

    // Find the first common ancestor in the paths
    val commonAncestor = startPath.intersect(endPath).headOption
    commonAncestor match {
      case Some(ancestor) =>
        val transfersToAncestor = startPath.indexOf(ancestor) + endPath.indexOf(ancestor)
        transfersToAncestor
      case None => -1 // Should not happen if input is valid
    }
  }

  def findPathToCOM(orbitMap: Map[String, String], objectName: String): List[String] = {
    val path = mutable.ListBuffer[String]()
    var current = objectName
    while (orbitMap.contains(current)) {
      current = orbitMap(current)
      path += current
    }
    path.toList
  }
}
