
object Day8 extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.next
  val width = 25
  val height = 6
  val layerSize = width * height
  val layers = input.grouped(layerSize).toList

  val layerWithFewestZeros = layers.minBy(layer => layer.count(_ == '0'))
  val result = layerWithFewestZeros.count(_ == '1') * layerWithFewestZeros.count(_ == '2')

  println(result)
}
