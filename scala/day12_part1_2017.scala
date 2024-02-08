object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val pipes = input.map(_.split(" <-> ")).map(arr => (arr(0).toInt, arr(1).split(", ").map(_.toInt).toList)).toMap

  def findGroup(program: Int, visited: Set[Int]): Set[Int] = {
    if (visited.contains(program)) visited
    else {
      val connected = pipes(program)
      connected.foldLeft(visited + program)((acc, p) => findGroup(p, acc))
    }
  }

  val group0 = findGroup(0, Set())
  println(group0.size)
}