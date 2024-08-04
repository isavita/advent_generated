import java.io.File

data class Monkey(
    val items: MutableList<Long>,
    val operation: (Long) -> Long,
    val testDivisible: Int,
    val ifTrue: Int,
    val ifFalse: Int,
    var inspectedItems: Int = 0
)

fun parseMonkey(lines: List<String>): Monkey {
    val items = lines[1].split(": ")[1].split(", ").map { it.toLong() }.toMutableList()
    val operation = lines[2].split(" = ")[1].let { expr ->
        val (left, op, right) = expr.split(" ")
        when (op) {
            "+" -> { l: Long -> l + right.toLong() }
            "*" -> { l: Long -> if (right == "old") l * l else l * right.toLong() }
            else -> throw IllegalArgumentException("Unknown operation: $op")
        }
    }
    val testDivisible = lines[3].split("by ")[1].toInt()
    val ifTrue = lines[4].split("monkey ")[1].toInt()
    val ifFalse = lines[5].split("monkey ")[1].toInt()
    return Monkey(items, operation, testDivisible, ifTrue, ifFalse)
}

fun readInput(filename: String): List<Monkey> {
    val lines = File(filename).readLines()
    val monkeys = mutableListOf<Monkey>()
    for (i in lines.indices step 7) {
        val monkeyLines = lines.subList(i, i + 6)
        monkeys.add(parseMonkey(monkeyLines))
    }
    return monkeys
}

fun simulateRounds(monkeys: List<Monkey>, rounds: Int) {
    repeat(rounds) {
        for (monkey in monkeys) {
            while (monkey.items.isNotEmpty()) {
                monkey.inspectedItems++
                var item = monkey.items.removeFirst()
                item = monkey.operation(item)
                item /= 3
                if (item % monkey.testDivisible == 0L) {
                    monkeys[monkey.ifTrue].items.add(item)
                } else {
                    monkeys[monkey.ifFalse].items.add(item)
                }
            }
        }
    }
}

fun main() {
    val monkeys = readInput("input.txt")
    simulateRounds(monkeys, 20)
    val inspectedItems = monkeys.map { it.inspectedItems }.sortedDescending()
    val monkeyBusiness = inspectedItems[0] * inspectedItems[1]
    println("Level of monkey business: $monkeyBusiness")
}