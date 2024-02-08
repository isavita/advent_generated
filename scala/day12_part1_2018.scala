
import scala.io.Source

object Main extends App {
  val filename = "input.txt"
  val source = Source.fromFile(filename)
  val lines = source.getLines().toList
  source.close()

  var initialState = ""
  var rules = Map[String, Char]()

  for (line <- lines) {
    if (line.contains("initial state")) {
      initialState = line.split(": ")(1)
    } else if (line.contains("=>")) {
      val parts = line.split(" => ")
      rules += (parts(0) -> parts(1)(0))
    }
  }

  var state = Map[Int, Char]()
  initialState.zipWithIndex.foreach { case (c, i) =>
    if (c == '#') {
      state += (i -> '#')
    }
  }

  for (_ <- 0 until 20) {
    var newState = Map[Int, Char]()
    val (minPot, maxPot) = minMaxKeys(state)
    for (i <- minPot - 2 to maxPot + 2) {
      var pattern = ""
      for (j <- i - 2 to i + 2) {
        pattern += (if (state.getOrElse(j, '.') == '#') "#" else ".")
      }
      if (rules.getOrElse(pattern, '.') == '#') {
        newState += (i -> '#')
      }
    }
    state = newState
  }

  val sum = state.keys.sum
  println(sum)

  def minMaxKeys(m: Map[Int, Char]): (Int, Int) = {
    var first = true
    var minKey = 0
    var maxKey = 0
    for (k <- m.keys) {
      if (first) {
        minKey = k
        maxKey = k
        first = false
      } else {
        if (k < minKey) {
          minKey = k
        }
        if (k > maxKey) {
          maxKey = k
        }
      }
    }
    (minKey, maxKey)
  }
}
