
import java.io.File
import kotlin.math.abs
import kotlin.math.max

fun main() {
    val input = File("input.txt").readText().trim()
    val result = solve(input)
    println(result)
}

fun solve(input: String): Int {
    val scanners = parseInput(input)

    val settled = mutableListOf(scanners[0].apply {
        absoluteCoords = relativeCoords
        fillAbsoluteCoordsMap()
    })
    val undetermined = scanners.drop(1).toMutableList()

    while (undetermined.isNotEmpty()) {
        var updated = false
        val iterator = undetermined.iterator()
        while (iterator.hasNext()) {
            val undet = iterator.next()
            val (maybeUpdated, ok) = findAbsoluteCoordsForScanner(undet, settled)
            if (ok) {
                settled.add(maybeUpdated)
                iterator.remove()
                updated = true
                break
            }
        }
        if (!updated) {
            throw IllegalStateException("No scanner could be settled")
        }
    }

    var furthest = 0
    for (i in settled.indices) {
        for (j in i + 1 until settled.size) {
            val s1 = settled[i]
            val s2 = settled[j]
            val manhattanDist = abs(s1.x - s2.x) + abs(s1.y - s2.y) + abs(s1.z - s2.z)
            furthest = max(furthest, manhattanDist)
        }
    }
    return furthest
}

data class Scanner(
    val number: Int,
    var x: Int = 0,
    var y: Int = 0,
    var z: Int = 0,
    var relativeCoords: List<Triple<Int, Int, Int>>,
    var rotations: List<List<Triple<Int, Int, Int>>> = emptyList(),
    var absoluteCoords: List<Triple<Int, Int, Int>> = emptyList(),
    var absoluteCoordsMap: MutableMap<Triple<Int, Int, Int>, Boolean> = mutableMapOf()
) {
    fun fillAbsoluteCoordsMap() {
        absoluteCoordsMap.clear()
        if (absoluteCoords.isEmpty()) {
            throw IllegalStateException("absolute coords not set for scanner $number")
        }
        absoluteCoords.forEach { absoluteCoordsMap[it] = true }
    }

    fun fillRotations() {
        val posX = relativeCoords
        val dir2 = posX.map { (x, y, z) -> Triple(x, -y, -z) }
        val dir3 = posX.map { (x, y, z) -> Triple(x, -z, y) }
        val dir4 = posX.map { (x, y, z) -> Triple(-y, -z, x) }
        val dir5 = posX.map { (x, y, z) -> Triple(-x, -z, -y) }
        val dir6 = posX.map { (x, y, z) -> Triple(y, -z, -x) }

        val sixRotations = listOf(
            posX, dir2,
            dir3, dir4,
            dir5, dir6
        )

        val finalRotations = mutableListOf<List<Triple<Int, Int, Int>>>()
        for (rotation in sixRotations) {
            val r2 = rotation.map { (x, y, z) -> Triple(-y, x, z) }
            val r3 = rotation.map { (x, y, z) -> Triple(-x, -y, z) }
            val r4 = rotation.map { (x, y, z) -> Triple(y, -x, z) }
            finalRotations.add(rotation)
            finalRotations.add(r2)
            finalRotations.add(r3)
            finalRotations.add(r4)
        }
        rotations = finalRotations
    }
}

fun findAbsoluteCoordsForScanner(
    undet: Scanner,
    settled: List<Scanner>
): Pair<Scanner, Boolean> {
    for (rotatedCoords in undet.rotations) {
        for (set in settled) {
            for (absCoord in set.absoluteCoords) {
                for (relativeCoord in rotatedCoords) {
                    val unsettledAbsoluteCoords = makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords)
                    var matchingCount = 0
                    for (ac in unsettledAbsoluteCoords) {
                        if (set.absoluteCoordsMap[ac] == true) {
                            matchingCount++
                        }
                    }
                    if (matchingCount >= 12) {
                        undet.relativeCoords = rotatedCoords
                        undet.absoluteCoords = unsettledAbsoluteCoords
                        undet.fillAbsoluteCoordsMap()
                        undet.x = absCoord.first - relativeCoord.first
                        undet.y = absCoord.second - relativeCoord.second
                        undet.z = absCoord.third - relativeCoord.third
                        return undet to true
                    }
                }
            }
        }
    }
    return undet to false
}

fun makeAbsoluteCoordsList(
    absolute: Triple<Int, Int, Int>,
    relative: Triple<Int, Int, Int>,
    relativeCoords: List<Triple<Int, Int, Int>>
): List<Triple<Int, Int, Int>> {
    val diff = Triple(
        absolute.first - relative.first,
        absolute.second - relative.second,
        absolute.third - relative.third
    )
    return relativeCoords.map { (x, y, z) ->
        Triple(
            diff.first + x,
            diff.second + y,
            diff.third + z
        )
    }
}

fun parseInput(input: String): List<Scanner> {
    return input.split("\n\n").map { rawScanner ->
        val lines = rawScanner.split("\n")
        val number = lines[0].substringAfter("scanner ").substringBefore(" ---").toInt()
        val coords = lines.drop(1).map { line ->
            val (x, y, z) = line.split(",").map { it.toInt() }
            Triple(x, y, z)
        }
        Scanner(
            number = number,
            relativeCoords = coords
        ).apply { fillRotations() }
    }
}
