
import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString
    println(sumAllNumbers(input))
    println(sumWithoutRed(input))
  }

  // Part 1: just grab all -?\d+ and sum
  def sumAllNumbers(s: String): Long =
    """-?\d+""".r.findAllIn(s).map(_.toLong).sum

  // Part 2: full JSON walk, ignore any object with a "red" value
  def sumWithoutRed(s: String): Long = {
    def skipSpaces(i: Int): Int =
      Iterator.iterate(i)(_ + 1).dropWhile(j => j < s.length && s(j).isWhitespace).next()

    def parseNumber(i: Int): (Long, Int) = {
      val sb = new StringBuilder
      var j = i
      if (s(j) == '-') { sb += '-'; j += 1 }
      while (j < s.length && s(j).isDigit) { sb += s(j); j += 1 }
      (sb.toString.toLong, j)
    }

    def parseString(i: Int): (String, Int) = {
      // assumes s(i) == '"'
      val sb = new StringBuilder
      var j = i + 1
      while (j < s.length) {
        s(j) match {
          case '"'  => return (sb.toString, j + 1)
          case '\\' =>
            if (j + 1 < s.length) {
              sb += s(j + 1)
              j += 2
            } else j += 1
          case c    =>
            sb += c
            j += 1
        }
      }
      // malformed, but just return
      (sb.toString, j)
    }

    // returns (sum, nextIndex)
    def parseValue(i0: Int): (Long, Int) = {
      var i = skipSpaces(i0)
      if (i >= s.length) return (0L, i)
      s(i) match {
        case '{' => parseObject(i)
        case '[' => parseArray(i)
        case '"' =>
          // string in array/context -- no contribution
          val (_, nxt) = parseString(i)
          (0L, nxt)
        case c if c == '-' || c.isDigit =>
          parseNumber(i)
        case _ =>
          // literals true,false,null -- ignore
          (0L, i + 1)
      }
    }

    def parseArray(i0: Int): (Long, Int) = {
      // s(i0) == '['
      var sum = 0L
      var i   = i0 + 1
      i = skipSpaces(i)
      if (i < s.length && s(i) == ']') return (0L, i + 1)
      var first = true
      while (i < s.length) {
        if (!first) i = skipSpaces(i) match {
          case j if s(j) == ',' => j + 1
          case j                 => j
        }
        first = false
        val (sub, nxt) = parseValue(i)
        sum += sub
        i = skipSpaces(nxt)
        if (i < s.length && s(i) == ']') return (sum, i + 1)
      }
      (sum, i)
    }

    def parseObject(i0: Int): (Long, Int) = {
      // s(i0) == '{'
      var sum    = 0L
      var hasRed = false
      var i      = i0 + 1
      i = skipSpaces(i)
      if (i < s.length && s(i) == '}') return (0L, i + 1)
      var first = true
      while (i < s.length) {
        if (!first) i = skipSpaces(i) match {
          case j if s(j) == ',' => j + 1
          case j                 => j
        }
        first = false
        // parse key
        val (key, afterKey) = parseString(skipSpaces(i))
        i = skipSpaces(afterKey)
        // skip colon
        if (i < s.length && s(i) == ':') i += 1
        i = skipSpaces(i)
        // parse value
        if (i < s.length && s(i) == '"') {
          val (str, nxt) = parseString(i)
          if (str == "red") hasRed = true
          i = nxt
        } else {
          val (sub, nxt) = parseValue(i)
          sum += sub
          i = nxt
        }
        i = skipSpaces(i)
        if (i < s.length && s(i) == '}') {
          return ((if (hasRed) 0L else sum), i + 1)
        }
      }
      // fallback
      ((if (hasRed) 0L else sum), i)
    }

    parseValue(0)._1
  }
}
