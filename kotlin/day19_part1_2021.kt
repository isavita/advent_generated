import java.io.File

data class Scanner(
    val number: Int,
    var x: Int,
    var y: Int,
    var z: Int,
    val relativeCoords: List<Triple<Int, Int, Int>>,
    var absoluteCoords: List<Triple<Int, Int, Int>>,
    var absoluteCoordsMap: MutableMap<Triple<Int, Int, Int>, Boolean>,
    var rotations: List<List<Triple<Int, Int, Int>>>
)

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
        for ((i, undet) in undetermined.withIndex()) {
            val (maybeUpdated, ok) = findAbsoluteCoordsForScanner(undet, settled)
            if (ok) {
                settled.add(maybeUpdated)
                undetermined.removeAt(i)
                break
            }
        }
    }

    val allBeacons = mutableMapOf<Triple<Int, Int, Int>, Boolean>()
    settled.forEach { s ->
        s.absoluteCoordsMap.keys.forEach { c ->
            allBeacons[c] = true
        }
    }

    return allBeacons.size
}

fun Scanner.fillAbsoluteCoordsMap() {
    absoluteCoordsMap = mutableMapOf()
    if (absoluteCoords.isEmpty()) {
        throw IllegalStateException("absolute coords not set for scanner $number")
    }
    absoluteCoords.forEach { ac ->
        absoluteCoordsMap[ac] = true
    }
}

fun Scanner.fillRotations() {
    val posX = relativeCoords
    val dir2 = mutableListOf<Triple<Int, Int, Int>>()
    val dir3 = mutableListOf<Triple<Int, Int, Int>>()
    val dir4 = mutableListOf<Triple<Int, Int, Int>>()
    val dir5 = mutableListOf<Triple<Int, Int, Int>>()
    val dir6 = mutableListOf<Triple<Int, Int, Int>>()

    posX.forEach { (x, y, z) ->
        dir2.add(Triple(x, -y, -z))
        dir3.add(Triple(x, -z, y))
        dir4.add(Triple(-y, -z, x))
        dir5.add(Triple(-x, -z, -y))
        dir6.add(Triple(y, -z, -x))
    }

    val sixRotations = listOf(posX, dir2, dir3, dir4, dir5, dir6)

    rotations = sixRotations.flatMap { rotation ->
        val r2 = mutableListOf<Triple<Int, Int, Int>>()
        val r3 = mutableListOf<Triple<Int, Int, Int>>()
        val r4 = mutableListOf<Triple<Int, Int, Int>>()

        rotation.forEach { (x, y, z) ->
            r2.add(Triple(-y, x, z))
            r3.add(Triple(-x, -y, z))
            r4.add(Triple(y, -x, z))
        }

        listOf(rotation, r2, r3, r4)
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
                    unsettledAbsoluteCoords.forEach { ac ->
                        if (set.absoluteCoordsMap[ac] == true) {
                            matchingCount++
                        }
                    }

                    if (matchingCount >= 12) {
                        undet.relativeCoords.toMutableList().apply {
                            clear()
                            addAll(rotatedCoords)
                        }
                        undet.absoluteCoords = unsettledAbsoluteCoords
                        undet.fillAbsoluteCoordsMap()
                        undet.x = absCoord.first - relativeCoord.first
                        undet.y = absCoord.second - relativeCoord.second
                        undet.z = absCoord.third - relativeCoord.third
                        return Pair(undet, true)
                    }
                }
            }
        }
    }
    return Pair(undet, false)
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
        Triple(diff.first + x, diff.second + y, diff.third + z)
    }
}

fun parseInput(input: String): List<Scanner> {
    return input.split("\n\n").map { rawScanner ->
        val lines = rawScanner.split("\n")
        val number = lines[0].split(" ")[2].toInt()

        val coords = lines.drop(1).map { line ->
            val (x, y, z) = line.split(",").map { it.toInt() }
            Triple(x, y, z)
        }

        Scanner(
            number = number,
            x = 0,
            y = 0,
            z = 0,
            relativeCoords = coords,
            absoluteCoords = emptyList(),
            absoluteCoordsMap = mutableMapOf(),
            rotations = emptyList()
        ).apply { fillRotations() }
    }
}