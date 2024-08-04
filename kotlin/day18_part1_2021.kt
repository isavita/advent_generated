import java.io.File

data class SnailNumber(var value: Int = -1, var left: SnailNumber? = null, var right: SnailNumber? = null) {
    fun isRegular() = left == null && right == null

    fun add(other: SnailNumber): SnailNumber {
        val newNumber = SnailNumber(left = this, right = other)
        return newNumber.reduce()
    }

    fun reduce(): SnailNumber {
        while (true) {
            val (exploded, _, _) = explode(0)
            if (exploded) continue
            if (!split()) break
        }
        return this
    }

    fun explode(depth: Int): Triple<Boolean, Int, Int> {
        if (isRegular()) return Triple(false, 0, 0)
        if (depth == 4) {
            val leftValue = left!!.value
            val rightValue = right!!.value
            left = null
            right = null
            value = 0
            return Triple(true, leftValue, rightValue)
        }
        val (exploded, leftValue, rightValue) = left!!.explode(depth + 1)
        if (exploded) {
            if (rightValue > 0 && right != null) right!!.addLeft(rightValue)
            return Triple(true, leftValue, 0)
        }
        val (exploded2, leftValue2, rightValue2) = right!!.explode(depth + 1)
        if (exploded2) {
            if (leftValue2 > 0 && left != null) left!!.addRight(leftValue2)
            return Triple(true, 0, rightValue2)
        }
        return Triple(false, 0, 0)
    }

    fun addLeft(value: Int) {
        if (isRegular()) this.value += value else left!!.addLeft(value)
    }

    fun addRight(value: Int) {
        if (isRegular()) this.value += value else right!!.addRight(value)
    }

    fun split(): Boolean {
        if (isRegular()) {
            if (value >= 10) {
                left = SnailNumber(value / 2)
                right = SnailNumber((value + 1) / 2)
                value = -1
                return true
            }
            return false
        }
        return left!!.split() || right!!.split()
    }

    fun magnitude(): Int = if (isRegular()) value else 3 * left!!.magnitude() + 2 * right!!.magnitude()
}

fun parseSnailNumber(input: String): SnailNumber {
    if (input[0] != '[') return SnailNumber(value = input.toInt())
    var balance = 0
    var splitIndex = 0
    for (i in 1 until input.length - 1) {
        when (input[i]) {
            '[' -> balance++
            ']' -> balance--
            ',' -> if (balance == 0) {
                splitIndex = i
                break
            }
        }
    }
    val left = parseSnailNumber(input.substring(1, splitIndex))
    val right = parseSnailNumber(input.substring(splitIndex + 1, input.length - 1))
    return SnailNumber(left = left, right = right)
}

fun main() {
    val lines = File("input.txt").readLines()
    val snailNumbers = lines.map { parseSnailNumber(it) }
    if (snailNumbers.isEmpty()) {
        println("No snailfish numbers found in the file.")
        return
    }
    var result = snailNumbers[0]
    for (i in 1 until snailNumbers.size) {
        result = result.add(snailNumbers[i])
    }
    println(result.magnitude())
}