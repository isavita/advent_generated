
import scala.io.Source
import java.security.MessageDigest

case class Point(x: Int, y: Int, path: String)

def main(args: Array[String]): Unit = {
  val passcode = readPasscode("input.txt")
  val path = findShortestPath(passcode)
  println(path)
}

def readPasscode(filename: String): String = {
  val bufferedSource = Source.fromFile(filename)
  val passcode = bufferedSource.getLines().next()
  bufferedSource.close()
  passcode
}

def findShortestPath(passcode: String): String = {
  var queue = List(Point(0, 0, ""))
  while (queue.nonEmpty) {
    val point = queue.head
    queue = queue.tail

    if (point.x == 3 && point.y == 3) {
      return point.path
    }

    for (dir <- getOpenDoors(passcode, point.path)) {
      val nextPoint = dir match {
        case "U" => Point(point.x, point.y - 1, point.path + dir)
        case "D" => Point(point.x, point.y + 1, point.path + dir)
        case "L" => Point(point.x - 1, point.y, point.path + dir)
        case "R" => Point(point.x + 1, point.y, point.path + dir)
      }

      if (nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4) {
        queue = queue :+ nextPoint
      }
    }
  }
  "No path found"
}

def getOpenDoors(passcode: String, path: String): List[String] = {
  val hash = md5Hash(passcode + path)
  var doors = List[String]()
  if (hash(0) >= 'b' && hash(0) <= 'f') doors :+= "U"
  if (hash(1) >= 'b' && hash(1) <= 'f') doors :+= "D"
  if (hash(2) >= 'b' && hash(2) <= 'f') doors :+= "L"
  if (hash(3) >= 'b' && hash(3) <= 'f') doors :+= "R"
  doors
}

def md5Hash(input: String): String = {
  val md = MessageDigest.getInstance("MD5")
  md.digest(input.getBytes).map("%02x".format(_)).mkString
}
