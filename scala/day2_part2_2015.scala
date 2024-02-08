
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val totalWrappingPaper = input.map { line =>
    val Array(l, w, h) = line.split("x").map(_.toInt)
    val sides = List(l*w, w*h, h*l)
    2*l*w + 2*w*h + 2*h*l + sides.min
  }.sum

  val totalRibbon = input.map { line =>
    val Array(l, w, h) = line.split("x").map(_.toInt)
    val bow = l * w * h
    val smallestPerimeter = List(2*l+2*w, 2*w+2*h, 2*h+2*l).min
    smallestPerimeter + bow
  }.sum

  println(totalWrappingPaper)
  println(totalRibbon)
}
