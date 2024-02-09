import java.io.File

fun main(args: Array<String>) {
    val inputData = mutableListOf<Int>()
    File("input.txt").forEachLine {
        val values = it.split(",")
        values.forEach { value ->
            inputData.add(value.toInt())
        }
    }

    inputData[1] = 12
    inputData[2] = 2

    val result = executeProgram(inputData)

    println(result)
}

fun executeProgram(data: MutableList<Int>): Int {
    var i = 0
    while (i < data.size - 3) {
        val pos1 = data[i + 1]
        val pos2 = data[i + 2]
        val pos3 = data[i + 3]
        when (data[i]) {
            1 -> {
                val sum = data[pos1] + data[pos2]
                data[pos3] = sum
            }
            2 -> {
                val product = data[pos1] * data[pos2]
                data[pos3] = product
            }
            99 -> return data[0]
            else -> throw IllegalArgumentException("Invalid opcode")
        }
        i += 4
    }

    return data[0]
}