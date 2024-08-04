import java.io.File

fun getValue(arg: String, registers: MutableMap<String, Long>): Long {
    return arg.toLongOrNull() ?: registers[arg] ?: 0
}

fun main() {
    val instructions = File("input.txt").readLines().map { it.split(" ") }
    val registers0 = mutableMapOf("p" to 0L)
    val registers1 = mutableMapOf("p" to 1L)
    val queue0 = mutableListOf<Long>()
    val queue1 = mutableListOf<Long>()
    var sendCount1 = 0
    var i0 = 0
    var i1 = 0
    var deadlock0: Boolean
    var deadlock1: Boolean

    do {
        deadlock0 = true
        deadlock1 = true

        while (i0 < instructions.size) {
            val instruction = instructions[i0]
            when (instruction[0]) {
                "snd" -> queue1.add(getValue(instruction[1], registers0))
                "set" -> registers0[instruction[1]] = getValue(instruction[2], registers0)
                "add" -> registers0[instruction[1]] = registers0[instruction[1]]!! + getValue(instruction[2], registers0)
                "mul" -> registers0[instruction[1]] = registers0[instruction[1]]!! * getValue(instruction[2], registers0)
                "mod" -> registers0[instruction[1]] = registers0[instruction[1]]!! % getValue(instruction[2], registers0)
                "rcv" -> {
                    if (queue0.isEmpty()) break
                    registers0[instruction[1]] = queue0.removeAt(0)
                }
                "jgz" -> if (getValue(instruction[1], registers0) > 0) {
                    i0 += getValue(instruction[2], registers0).toInt() - 1
                }
            }
            i0++
            deadlock0 = false
        }

        while (i1 < instructions.size) {
            val instruction = instructions[i1]
            when (instruction[0]) {
                "snd" -> {
                    queue0.add(getValue(instruction[1], registers1))
                    sendCount1++
                }
                "set" -> registers1[instruction[1]] = getValue(instruction[2], registers1)
                "add" -> registers1[instruction[1]] = registers1[instruction[1]]!! + getValue(instruction[2], registers1)
                "mul" -> registers1[instruction[1]] = registers1[instruction[1]]!! * getValue(instruction[2], registers1)
                "mod" -> registers1[instruction[1]] = registers1[instruction[1]]!! % getValue(instruction[2], registers1)
                "rcv" -> {
                    if (queue1.isEmpty()) break
                    registers1[instruction[1]] = queue1.removeAt(0)
                }
                "jgz" -> if (getValue(instruction[1], registers1) > 0) {
                    i1 += getValue(instruction[2], registers1).toInt() - 1
                }
            }
            i1++
            deadlock1 = false
        }
    } while (!(deadlock0 && deadlock1))

    println(sendCount1)
}