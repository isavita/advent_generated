import java.io.File

fun findCombinations(containers: List<Int>, target: Int, index: Int, count: Int, minCount: Int, ways: Int): Pair<Int, Int> {
    var newMinCount = minCount
    var newWays = ways
    if (target == 0) {
        if (newMinCount == 0 || count < newMinCount) {
            newMinCount = count
            newWays = 1
        } else if (count == newMinCount) {
            newWays++
        }
        return Pair(newMinCount, newWays)
    }
    if (target < 0 || index >= containers.size) {
        return Pair(newMinCount, newWays)
    }
    val includeResult = findCombinations(containers, target - containers[index], index + 1, count + 1, newMinCount, newWays)
    val excludeResult = findCombinations(containers, target, index + 1, count, includeResult.first, includeResult.second)
    return excludeResult
}

fun main(args: Array<String>) {
    val containers = File("input.txt").readLines().map { it.toInt() }

    val (minCount, ways) = findCombinations(containers, 150, 0, 0, 0, 0)
    println(ways)
}