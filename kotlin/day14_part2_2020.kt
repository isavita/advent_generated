import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    println("Part 1: ${part1(input)}")
    println("Part 2: ${part2(input)}")
}

fun part1(input: List<String>): Long {
    val memory = mutableMapOf<Long, Long>()
    var mask = ""

    for (line in input) {
        if (line.startsWith("mask = ")) {
            mask = line.substringAfter("mask = ")
        } else {
            val memMatch = Regex("""mem\[(\d+)\] = (\d+)""").find(line)
            if (memMatch != null) {
                val (address, valueStr) = memMatch.destructured
                val value = valueStr.toLong()
                memory[address.toLong()] = applyMaskToValue(value, mask)
            }
        }
    }

    return memory.values.sum()
}

fun applyMaskToValue(value: Long, mask: String): Long {
    var result = value
    for ((index, bit) in mask.withIndex()) {
        when (bit) {
            '1' -> result = result or (1L shl (35 - index))
            '0' -> result = result and (1L shl (35 - index)).inv()
        }
    }
    return result
}

fun part2(input: List<String>): Long {
    val memory = mutableMapOf<Long, Long>()
    var mask = ""

    for (line in input) {
        if (line.startsWith("mask = ")) {
            mask = line.substringAfter("mask = ")
        } else {
            val memMatch = Regex("""mem\[(\d+)\] = (\d+)""").find(line)
            if (memMatch != null) {
                val (addressStr, valueStr) = memMatch.destructured
                val address = addressStr.toLong()
                val value = valueStr.toLong()
                val addresses = generateAddresses(address, mask)
                for (addr in addresses) {
                    memory[addr] = value
                }
            }
        }
    }

    return memory.values.sum()
}

fun generateAddresses(address: Long, mask: String): List<Long> {
    val addresses = mutableListOf<Long>()
    val baseAddress = applyMaskToAddress(address, mask)
    val floatingBits = mask.mapIndexedNotNull { index, bit -> if (bit == 'X') 35 - index else null }

    for (i in 0 until (1 shl floatingBits.size)) {
        var addr = baseAddress
        for (j in floatingBits.indices) {
            if ((i and (1 shl j)) != 0) {
                addr = addr or (1L shl floatingBits[j])
            } else {
                addr = addr and (1L shl floatingBits[j]).inv()
            }
        }
        addresses.add(addr)
    }

    return addresses
}

fun applyMaskToAddress(address: Long, mask: String): Long {
    var result = address
    for ((index, bit) in mask.withIndex()) {
        when (bit) {
            '1' -> result = result or (1L shl (35 - index))
            'X' -> result = result and (1L shl (35 - index)).inv()
        }
    }
    return result
}