
object Day7 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList
  val pattern = "Step (\\w) must be finished before step (\\w) can begin.".r

  val dependencies = input.map {
    case pattern(parent, child) => (parent.head, child.head)
  }

  val allSteps = dependencies.flatMap { case (parent, child) => List(parent, child) }.distinct.sorted

  def findOrder(dependencies: List[(Char, Char)], available: List[Char], completed: List[Char]): List[Char] = {
    if (available.isEmpty) completed
    else {
      val nextStep = available.filterNot(step => dependencies.exists(_._2 == step)).min
      val newDependencies = dependencies.filterNot(_._1 == nextStep)
      val newAvailable = available.filterNot(_ == nextStep)
      findOrder(newDependencies, newAvailable, completed :+ nextStep)
    }
  }

  val order = findOrder(dependencies, allSteps, List())

  println(order.mkString)
}
