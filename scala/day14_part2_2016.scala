import java.security.MessageDigest
import scala.io.Source
import scala.collection.mutable.Queue

object Main {
  def md5(s: String): String = {
    val m = MessageDigest.getInstance("MD5")
    m.update(s.getBytes)
    val bytes = m.digest()
    val sb = new StringBuilder
    for (b <- bytes) sb.append(f"${b & 0xff}%02x")
    sb.toString
  }
  def stretch(s: String): String = {
    var h = md5(s)
    for (_ <- 1 to 2016) h = md5(h)
    h
  }
  def triple(s: String): Option[Char] = {
    var i = 0
    while (i < s.length - 2) {
      val c = s(i)
      if (s(i + 1) == c && s(i + 2) == c) return Some(c)
      i += 1
    }
    None
  }
  def main(args: Array[String]): Unit = {
    val salt = Source.fromFile("input.txt").mkString.trim
    var idx = 0
    var keys = 0
    var current = stretch(salt + "0")
    val q = Queue[String]()
    for (i <- 1 to 1000) q.enqueue(stretch(salt + i))
    while (true) {
      triple(current) match {
        case Some(c) if q.exists(_.contains(c.toString * 5)) =>
          keys += 1
          if (keys == 64) { println(idx); return }
        case _ =>
      }
      current = q.dequeue()
      idx += 1
      q.enqueue(stretch(salt + (idx + 1000)))
    }
  }
}