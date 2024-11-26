
import scala.io.Source

object NoSpaceLeftOnDevice {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines().toList
    val fileSystem = buildFileSystem(lines)
    val smallDirectories = findSmallDirectories(fileSystem)
    val totalSize = smallDirectories.map(_._2).sum
    println(s"Sum of small directories: $totalSize")
  }

  def buildFileSystem(lines: List[String]): Map[String, Long] = {
    var currentPath = List[String]()
    val dirSizes = scala.collection.mutable.Map[String, Long]().withDefaultValue(0L)

    for (line <- lines) {
      line.split(" ") match {
        case Array("$", "cd", "/") => 
          currentPath = List("/")
        case Array("$", "cd", "..") => 
          currentPath = currentPath.dropRight(1)
        case Array("$", "cd", dir) => 
          currentPath = currentPath :+ dir
        case Array("$", "ls") => 
          // Do nothing
        case Array("dir", _) => 
          // Do nothing
        case Array(size, _) if size.forall(_.isDigit) => 
          val fileSize = size.toLong
          for (i <- 1 to currentPath.length) {
            val path = currentPath.take(i).mkString("/")
            dirSizes(path) += fileSize
          }
        case _ => 
          // Ignore other lines
      }
    }

    dirSizes.toMap
  }

  def findSmallDirectories(fileSystem: Map[String, Long]): List[(String, Long)] = {
    fileSystem.filter { case (_, size) => size <= 100000 }.toList
  }
}
