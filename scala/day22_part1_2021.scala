
import scala.io.Source

case class RebootStep(action: String, xStart: Int, xEnd: Int, yStart: Int, yEnd: Int, zStart: Int, zEnd: Int)

object Main extends App {
  val filename = "input.txt"
  val rebootSteps = Source.fromFile(filename).getLines
    .filter(_.nonEmpty)
    .map(parseRebootStep)
    .toList

  val minCoord = -50
  val maxCoord = 50
  val cubeGrid = createCubeGrid(minCoord, maxCoord)
  executeRebootSteps(cubeGrid, rebootSteps)
  val onCubes = countOnCubes(cubeGrid)

  println(onCubes)

  def parseRebootStep(line: String): RebootStep = {
    val parts = line.split(" ")
    val action = parts(0)
    val xRange = parts(1).split(",")(0).substring(2).split("\\.\\.")
    val yRange = parts(1).split(",")(1).substring(2).split("\\.\\.")
    val zRange = parts(1).split(",")(2).substring(2).split("\\.\\.")

    val xStart = xRange(0).toInt
    val xEnd = xRange(1).toInt
    val yStart = yRange(0).toInt
    val yEnd = yRange(1).toInt
    val zStart = zRange(0).toInt
    val zEnd = zRange(1).toInt

    RebootStep(action, xStart, xEnd, yStart, yEnd, zStart, zEnd)
  }

  def createCubeGrid(minCoord: Int, maxCoord: Int): Array[Array[Array[Boolean]]] = {
    val gridSize = maxCoord - minCoord + 1
    Array.fill(gridSize, gridSize, gridSize)(false)
  }

  def executeRebootSteps(cubeGrid: Array[Array[Array[Boolean]]], rebootSteps: List[RebootStep]): Unit = {
    for (step <- rebootSteps) {
      if (!(step.xStart >= -50 && step.xEnd <= 50 && step.yStart >= -50 && step.yEnd <= 50 && step.zStart >= -50 && step.zEnd <= 50)) {
        // Skip invalid steps
      } else {
        for (x <- step.xStart to step.xEnd; y <- step.yStart to step.yEnd; z <- step.zStart to step.zEnd) {
          cubeGrid(x + 50)(y + 50)(z + 50) = step.action == "on"
        }
      }
    }
  }

  def countOnCubes(cubeGrid: Array[Array[Array[Boolean]]]): Int = {
    cubeGrid.map(_.flatten).flatten.count(_ == true)
  }
}
