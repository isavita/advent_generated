import java.io.File

fun main() {
    val file = File("input.txt")
    val moves = file.readText().split(",")

    var programs = "abcdefghijklmnop".toCharArray()
    val initial = String(programs)
    var cycleLen = 0

    for (i in 0 until 1000000000) {
        for (move in moves) {
            when (move[0]) {
                's' -> {
                    val x = move.substring(1).toInt()
                    spin(programs, x)
                }
                'x' -> {
                    val positions = move.substring(1).split("/")
                    val A = positions[0].toInt()
                    val B = positions[1].toInt()
                    exchange(programs, A, B)
                }
                'p' -> {
                    val positions = move.substring(1).split("/")
                    val A = positions[0][0]
                    val B = positions[1][0]
                    partner(programs, A, B)
                }
            }
        }

        if (String(programs) == initial) {
            cycleLen = i + 1
            break
        }
    }

    programs = initial.toCharArray()

    for (i in 0 until 1000000000 % cycleLen) {
        for (move in moves) {
            when (move[0]) {
                's' -> {
                    val x = move.substring(1).toInt()
                    spin(programs, x)
                }
                'x' -> {
                    val positions = move.substring(1).split("/")
                    val A = positions[0].toInt()
                    val B = positions[1].toInt()
                    exchange(programs, A, B)
                }
                'p' -> {
                    val positions = move.substring(1).split("/")
                    val A = positions[0][0]
                    val B = positions[1][0]
                    partner(programs, A, B)
                }
            }
        }
    }

    println(String(programs))
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
    var indexA = -1
    var indexB = -1
    for (i in programs.indices) {
        if (programs[i] == A) indexA = i
        if (programs[i] == B) indexB = i
    }
    exchange(programs, indexA, indexB)
}