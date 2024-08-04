import java.io.File
import java.util.regex.Pattern

fun main() {
    val sues = File("input.txt").readLines()
        .map { line ->
            val match = Pattern.compile("Sue (\\d+): (.*)").matcher(line).apply { find() }
            val id = match.group(1).toInt()
            val props = match.group(2).split(", ").map { prop ->
                val (name, value) = prop.split(": ")
                name to value.toInt()
            }.toSet()
            id to props
        }.toMap()

    val target = mapOf(
        "children" to 3,
        "cats" to 7,
        "samoyeds" to 2,
        "pomeranians" to 3,
        "akitas" to 0,
        "vizslas" to 0,
        "goldfish" to 5,
        "trees" to 3,
        "cars" to 2,
        "perfumes" to 1
    )

    val part1Answer = sues.entries.find { entry ->
        entry.value.all { prop ->
            target[prop.first] == prop.second
        }
    }?.key

    println("Part 1 answer: $part1Answer")

    val part2Answer = sues.entries.find { entry ->
        entry.value.all { prop ->
            when (prop.first) {
                "cats" -> target[prop.first]!! < prop.second
                "trees" -> target[prop.first]!! < prop.second
                "pomeranians" -> target[prop.first]!! > prop.second
                "goldfish" -> target[prop.first]!! > prop.second
                else -> target[prop.first] == prop.second
            }
        }
    }?.key

    println("Part 2 answer: $part2Answer")
}