fun main() {
    val input = mutableListOf<String>()
    input.addAll(java.io.File("input.txt").readLines())
    
    var result = 0
    input.forEach { line ->
        result += line.toInt()
    }
    
    println(result)
}