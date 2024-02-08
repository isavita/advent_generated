
object Main extends App {
    val instructions = scala.io.Source.fromFile("input.txt").getLines.toList
    val keypad = Array(
        Array('1', '2', '3'),
        Array('4', '5', '6'),
        Array('7', '8', '9')
    )
    
    var code = ""
    var x = 1
    var y = 1
    
    for (instruction <- instructions) {
        for (move <- instruction) {
            move match {
                case 'U' => if (y > 0) y -= 1
                case 'D' => if (y < 2) y += 1
                case 'L' => if (x > 0) x -= 1
                case 'R' => if (x < 2) x += 1
            }
        }
        code += keypad(y)(x)
    }
    
    println(code)
}
