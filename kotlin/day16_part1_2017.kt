import java.io.File

fun main(args: Array<String>) {
    val moves = File("input.txt").readLines()[0].split(",")

    var programs = "abcdefghijklmnop".toCharArray()

    moves.forEach { move ->
        when (move[0]) {
            's' -> spin(programs, move.substring(1).toInt())
            'x' -> {
                val positions = move.substring(1).split("/")
                exchange(programs, positions[0].toInt(), positions[1].toInt())
            }
            'p' -> {
                val positions = move.substring(1).split("/")
                partner(programs, positions[0][0], positions[1][0])
            }
        }
    }

    println(programs.joinToString(""))
}

fun spin(programs: CharArray, x: Int) {
    val n = programs.size
    val temp = programs.copyOf()

    for (i in 0 until n) {
        programs[(i + x) % n] = temp[i]
    }
}

fun exchange(programs: CharArray, A: Int, B: Int) {
    val temp = programs[A]
    programs[A] = programs[B]
    programs[B] = temp
}

fun partner(programs: CharArray, A: Char, B: Char) {
    val indexA = programs.indexOf(A)
    val indexB = programs.indexOf(B)
    exchange(programs, indexA, indexB)
}