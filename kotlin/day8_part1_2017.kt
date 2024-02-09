import java.io.File

fun main(args: Array<String>) {
    val file = File("input.txt")
    val registers = mutableMapOf<String, Int>()

    file.forEachLine {
        val parts = it.split(" ")
        val reg = parts[0]
        val op = parts[1]
        val amount = parts[2].toInt()
        val condReg = parts[4]
        val condOp = parts[5]
        val condVal = parts[6].toInt()

        var cond = false
        when (condOp) {
            ">" -> cond = registers.getOrDefault(condReg, 0) > condVal
            ">=" -> cond = registers.getOrDefault(condReg, 0) >= condVal
            "<" -> cond = registers.getOrDefault(condReg, 0) < condVal
            "<=" -> cond = registers.getOrDefault(condReg, 0) <= condVal
            "==" -> cond = registers.getOrDefault(condReg, 0) == condVal
            "!=" -> cond = registers.getOrDefault(condReg, 0) != condVal
        }

        if (cond) {
            when (op) {
                "inc" -> registers[reg] = registers.getOrDefault(reg, 0) + amount
                "dec" -> registers[reg] = registers.getOrDefault(reg, 0) - amount
            }
        }
    }

    val maxValue = registers.values.maxOrNull() ?: 0
    println(maxValue)
}