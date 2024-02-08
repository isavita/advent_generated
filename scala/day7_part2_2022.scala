import scala.io.Source

object Solution {
  def main(args: Array[String]): Unit = {
    val root = List("")
    var dirs = Map[String, Int]()
    var files = Map[String, Int]()
    var curr = List[String]()
    
    val source = Source.fromFile("input.txt")
    val lines = source.getLines().toList
    source.close()
    
    for (line <- lines) {
      val txt = line.split(" ")
      if (txt(0) == "$") {
        if (txt(1) == "cd") {
          if (txt(2) == "/") {
            curr = root
          } else if (txt(2) == "..") {
            curr = curr.init
          } else {
            curr = curr :+ txt(2)
          }
          dirs += (curr.mkString("/") -> 0)
        }
      } else {
        if (txt(0) != "dir") {
          files += (curr :+ txt(1)).mkString("/") -> txt(0).toInt
        }
      }
    }
    
    for ((f, s) <- files) {
      val path = f.split("/")
      for (i <- 1 until path.length) {
        dirs += (path.slice(0, i).mkString("/") -> (dirs.getOrElse(path.slice(0, i).mkString("/"), 0) + s))
      }
    }
    
    val sortedSizes = dirs.values.toList.sorted
    val total = 70000000
    val want = 30000000
    val available = total - dirs("")
    
    println(sortedSizes(sortedSizes.indexWhere(_ >= (want - available))))
  }
}