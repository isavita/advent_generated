import java.io.File

data class SnailNumber(var value: Int = 0, var left: SnailNumber? = null, var right: SnailNumber? = null) {
    fun isRegular() = left == null && right == null

    fun add(other: SnailNumber): SnailNumber {
        val newNumber = SnailNumber(left = this, right = other)
        return newNumber.reduce()
    }

    fun reduce(): SnailNumber {
        while (true) {
            val (exploded, leftValue, rightValue) = explode(0)
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
            right?.addLeft(rightValue)
            return Triple(true, leftValue, 0)
        }
        val (explodedRight, leftValueRight, rightValueRight) = right!!.explode(depth + 1)
        if (explodedRight) {
            left?.addRight(leftValueRight)
            return Triple(true, 0, rightValueRight)
        }
        return Triple(false, 0, 0)
    }

    fun addLeft(value: Int) {
        if (isRegular()) this.value += value
        else left!!.addLeft(value)
    }

    fun addRight(value: Int) {
        if (isRegular()) this.value += value
        else right!!.addRight(value)
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

    fun magnitude(): Int {
        if (isRegular()) return value
        return 3 * left!!.magnitude() + 2 * right!!.magnitude()
    }

    fun deepCopy(): SnailNumber {
        return if (isRegular()) SnailNumber(value) else SnailNumber(left = left!!.deepCopy(), right = right!!.deepCopy())
    }
}

fun parseSnailNumber(input: String): SnailNumber {
    val trimmed = input.trim()
    if (trimmed[0] != '[') return SnailNumber(trimmed.toInt())
    var balance = 0
    var splitIndex = 0
    for (i in 1 until trimmed.length - 1) {
        when (trimmed[i]) {
            '[' -> balance++
            ']' -> balance--
            ',' -> if (balance == 0) {
                splitIndex = i + 1
                break
            }
        }
    }
    val left = parseSnailNumber(trimmed.substring(1, splitIndex - 1))
    val right = parseSnailNumber(trimmed.substring(splitIndex, trimmed.length - 1))
    return SnailNumber(left = left, right = right)
}

fun main() {
    val snailNumbers = File("input.txt").readLines().map { parseSnailNumber(it) }
    var largestMagnitude = 0
    for (i in snailNumbers.indices) {
        for (j in snailNumbers.indices) {
            if (i == j) continue
            val sum1 = snailNumbers[i].deepCopy().add(snailNumbers[j].deepCopy()).magnitude()
            val sum2 = snailNumbers[j].deepCopy().add(snailNumbers[i].deepCopy()).magnitude()
            largestMagnitude = maxOf(largestMagnitude, sum1, sum2)
        }
    }
    println(largestMagnitude)
}