import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val registers = mutableMapOf<String, Int>()
    var highestValue = 0

    file.forEachLine {
        val parts = it.split(" ")
        val reg = parts[0]
        val op = parts[1]
        val amount = parts[2].toInt()
        val condReg = parts[4]
        val condOp = parts[5]
        val condVal = parts[6].toInt()

        val cond: Boolean = when (condOp) {
            ">" -> registers.getOrDefault(condReg, 0) > condVal
            ">=" -> registers.getOrDefault(condReg, 0) >= condVal
            "<" -> registers.getOrDefault(condReg, 0) < condVal
            "<=" -> registers.getOrDefault(condReg, 0) <= condVal
            "==" -> registers.getOrDefault(condReg, 0) == condVal
            "!=" -> registers.getOrDefault(condReg, 0) != condVal
            else -> false
        }

        if (cond) {
            when (op) {
                "inc" -> registers[reg] = registers.getOrDefault(reg, 0) + amount
                "dec" -> registers[reg] = registers.getOrDefault(reg, 0) - amount
            }

            if (registers[reg]!! > highestValue) {
                highestValue = registers[reg]!!
            }
        }
    }

    println(highestValue)
}