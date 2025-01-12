
import java.io.File
import java.math.BigInteger

data class Monkey(
    val items: MutableList<BigInteger> = mutableListOf(),
    var operation: (BigInteger) -> BigInteger = { it },
    var div: Int = 0,
    val next: IntArray = IntArray(2),
    var inspections: Int = 0
)

fun parse(s: String): Monkey {
    val m = Monkey()
    val lines = s.split("\n")
    lines[1].split(": ")[1].split(", ").forEach { m.items.add(it.toBigInteger()) }
    val f = lines[2].split("= ")[1].split(" ")
    when (f[1]) {
        "+" -> when (f[2]) {
            "old" -> m.operation = { old -> old + old }
            else -> m.operation = { old -> old + f[2].toBigInteger() }
        }
        "*" -> when (f[2]) {
            "old" -> m.operation = { old -> old * old }
            else -> m.operation = { old -> old * f[2].toBigInteger() }
        }
    }
    m.div = lines[3].split(" ").last().toInt()
    m.next[0] = lines[4].split(" ").last().toInt()
    m.next[1] = lines[5].split(" ").last().toInt()
    return m
}

fun monkeyBusiness(monkeys: List<Monkey>, rounds: Int, worry: Boolean): BigInteger {
    val div = monkeys.fold(BigInteger.ONE) { acc, m -> acc * m.div.toBigInteger() }

    repeat(rounds) {
        monkeys.forEach { m ->
            while (m.items.isNotEmpty()) {
                m.inspections++
                var item = m.operation(m.items.removeAt(0))
                if (worry) {
                    item %= div
                } else {
                    item /= BigInteger.valueOf(3)
                }
                val nextMonkey = if (item % m.div.toBigInteger() == BigInteger.ZERO) m.next[0] else m.next[1]
                monkeys[nextMonkey].items.add(item)
            }
        }
    }
    return monkeys.map { it.inspections.toBigInteger() }.sortedDescending().take(2).reduce { a, b -> a * b }
}

fun main() {
    val input = File("input.txt").readText()
    val monkeys = input.split("\n\n").map { parse(it) }
    println(monkeyBusiness(monkeys, 10000, true))
}
