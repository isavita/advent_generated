
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  val fabric = Array.ofDim[Int](1000, 1000)
  
  for (line <- input) {
    val parts = line.split(" ")
    val coords = parts(2).init.split(",")
    val size = parts(3).split("x")
    
    val x = coords(0).toInt
    val y = coords(1).toInt
    val width = size(0).toInt
    val height = size(1).toInt
    
    for (i <- x until x + width) {
      for (j <- y until y + height) {
        fabric(i)(j) += 1
      }
    }
  }
  
  var count = 0
  for (i <- 0 until 1000) {
    for (j <- 0 until 1000) {
      if (fabric(i)(j) > 1) {
        count += 1
      }
    }
  }
  
  println(count)
}
