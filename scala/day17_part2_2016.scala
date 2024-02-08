
import scala.io.Source
import java.security.MessageDigest

case class Point(x: Int, y: Int, path: String)

object Main extends App {
  val passcode = Source.fromFile("input.txt").getLines().next()
  val longestPathLength = findLongestPathLength(passcode)
  println(longestPathLength)

  def findLongestPathLength(passcode: String): Int = {
    var longest = 0
    var queue = List(Point(0, 0, ""))
    while (queue.nonEmpty) {
      val point = queue.head
      queue = queue.tail

      if (point.x == 3 && point.y == 3) {
        if (point.path.length > longest) {
          longest = point.path.length
        }
      } else {
        for (dir <- getOpenDoors(passcode, point.path)) {
          val nextPoint = dir match {
            case "U" => Point(point.x, point.y - 1, point.path + "U")
            case "D" => Point(point.x, point.y + 1, point.path + "D")
            case "L" => Point(point.x - 1, point.y, point.path + "L")
            case "R" => Point(point.x + 1, point.y, point.path + "R")
          }

          if (nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4) {
            queue = queue :+ nextPoint
          }
        }
      }
    }
    longest
  }

  def getOpenDoors(passcode: String, path: String): List[String] = {
    val hash = md5Hash(passcode + path)
    var doors = List[String]()
    if ('b' <= hash(0) && hash(0) <= 'f') doors :+= "U"
    if ('b' <= hash(1) && hash(1) <= 'f') doors :+= "D"
    if ('b' <= hash(2) && hash(2) <= 'f') doors :+= "L"
    if ('b' <= hash(3) && hash(3) <= 'f') doors :+= "R"
    doors
  }

  def md5Hash(input: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.getBytes)
    digest.map("%02x".format(_)).mkString
  }
}
