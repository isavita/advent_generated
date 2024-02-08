object Day3 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList
  val triangles = input.map(_.trim.split("\\s+").map(_.toInt).sorted)
  val possibleTriangles = triangles.count { case Array(a, b, c) => a + b > c }
  println(possibleTriangles)
}