
import scala.io.Source

object Main extends App {
  val instr = Source.fromFile("input.txt").getLines().toArray
  var a = 1L
  var b = 0L
  var i = 0
  while (i < instr.length) {
    val parts = instr(i).split(" ")
    parts(0) match {
      case "hlf" =>
        if (parts(1)(0) == 'a') a /= 2 else b /= 2
      case "tpl" =>
        if (parts(1)(0) == 'a') a *= 3 else b *= 3
      case "inc" =>
        if (parts(1)(0) == 'a') a += 1 else b += 1
      case "jmp" =>
        i += parts(1).toInt - 1
      case "jie" =>
        val r = if (parts(1)(0) == 'a') a else b
        if (r % 2 == 0) i += parts(2).toInt - 1
      case "jio" =>
        val r = if (parts(1)(0) == 'a') a else b
        if (r == 1) i += parts(2).toInt - 1
    }
    i += 1
  }
  println(b)
}
