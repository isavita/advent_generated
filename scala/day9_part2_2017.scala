
object Main extends App {
    val input = scala.io.Source.fromFile("input.txt").getLines.mkString
    var score = 0
    var garbageCount = 0
    var inGarbage = false
    var ignoreNext = false
    var depth = 0

    for (char <- input) {
        if (ignoreNext) {
            ignoreNext = false
        } else {
            char match {
                case '{' if !inGarbage => {
                    depth += 1
                    score += depth
                }
                case '}' if !inGarbage => {
                    depth -= 1
                }
                case '<' if !inGarbage => {
                    inGarbage = true
                }
                case '>' if inGarbage => {
                    inGarbage = false
                }
                case '!' => {
                    ignoreNext = true
                }
                case _ if inGarbage => {
                    garbageCount += 1
                }
                case _ =>
            }
        }
    }

    println(score)
    println(garbageCount)
}
