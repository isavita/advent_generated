
import scala.io.Source
import scala.collection.mutable

object Day7 {
  def parseInput(filename: String): Map[Char, Set[Char]] = {
    val dependencies = mutable.Map[Char, Set[Char]]().withDefaultValue(Set.empty)
    val steps = mutable.Set[Char]()

    Source.fromFile(filename).getLines().foreach { line =>
      val Array(_, dep, _, _, _, _, _, step, _, _) = line.split(" ")
      val depChar = dep.head
      val stepChar = step.head
      
      dependencies(stepChar) += depChar
      steps += depChar
      steps += stepChar
    }

    dependencies.toMap
  }

  def part1(dependencies: Map[Char, Set[Char]]): String = {
    val allSteps = dependencies.keySet ++ dependencies.values.flatten
    val remainingSteps = mutable.Set[Char]() ++ allSteps
    val completed = mutable.ListBuffer[Char]()

    while (remainingSteps.nonEmpty) {
      val availableSteps = remainingSteps.filter { step =>
        dependencies.getOrElse(step, Set.empty).subsetOf(completed.toSet)
      }.toList.sorted

      val nextStep = availableSteps.head
      completed += nextStep
      remainingSteps -= nextStep
    }

    completed.mkString
  }

  def part2(dependencies: Map[Char, Set[Char]], workers: Int = 5, baseTime: Int = 60): Int = {
    val allSteps = dependencies.keySet ++ dependencies.values.flatten
    val remainingSteps = mutable.Set[Char]() ++ allSteps
    val completed = mutable.Set[Char]()
    val workerTasks = mutable.Map[Int, (Char, Int)]()

    def stepTime(step: Char): Int = baseTime + (step - 'A' + 1)

    var time = 0
    while (remainingSteps.nonEmpty || workerTasks.nonEmpty) {
      // Complete tasks
      workerTasks.filter(_._2._2 <= time).foreach { case (worker, (step, _)) =>
        completed += step
        workerTasks.remove(worker)
      }

      // Find available steps
      val availableSteps = remainingSteps.filter { step =>
        dependencies.getOrElse(step, Set.empty).subsetOf(completed)
      }.toList.sorted

      // Assign tasks to idle workers
      for (step <- availableSteps if workerTasks.size < workers) {
        val worker = (0 until workers).find(!workerTasks.contains(_)).get
        workerTasks(worker) = (step, time + stepTime(step))
        remainingSteps -= step
      }

      time += 1
    }

    time - 1
  }

  def main(args: Array[String]): Unit = {
    val dependencies = parseInput("input.txt")
    println(s"Part 1: ${part1(dependencies)}")
    println(s"Part 2: ${part2(dependencies)}")
  }
}
