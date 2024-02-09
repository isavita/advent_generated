import java.io.File

fun main() {
    val input = File("input.txt").readText().trim().split(",")
    
    var x = 0
    var y = 0
    var z = 0
    
    var maxDistance = 0
    
    for (direction in input) {
        when (direction) {
            "n" -> { y++; z-- }
            "ne" -> { x++; z-- }
            "se" -> { x++; y-- }
            "s" -> { y--; z++ }
            "sw" -> { x--; z++ }
            "nw" -> { x--; y++ }
        }
        
        val distance = (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2
        if (distance > maxDistance) {
            maxDistance = distance
        }
    }
    
    println((Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2)
}