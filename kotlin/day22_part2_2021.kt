import java.io.File

data class Cuboid(val state: Boolean, val xRange: IntRange, val yRange: IntRange, val zRange: IntRange)

fun main() {
    val input = File("input.txt").readLines()
    val rebootSteps = input.map { line ->
        val parts = line.split(" ")
        val state = parts[0] == "on"
        val ranges = parts[1].split(",").map { it.split("=")[1].split("..").map { num -> num.toInt() } }
        Cuboid(state, ranges[0][0]..ranges[0][1], ranges[1][0]..ranges[1][1], ranges[2][0]..ranges[2][1])
    }

    // Part 1: Considering only cubes in the region x=-50..50,y=-50..50,z=-50..50
    val part1CubesOn = calculateCubesOn(rebootSteps, -50..50, -50..50, -50..50)
    println("Part 1: $part1CubesOn cubes are on.")

    // Part 2: Considering all cubes
    val part2CubesOn = calculateCubesOn(rebootSteps)
    println("Part 2: $part2CubesOn cubes are on.")
}

fun calculateCubesOn(steps: List<Cuboid>, xRange: IntRange = Int.MIN_VALUE..Int.MAX_VALUE, yRange: IntRange = Int.MIN_VALUE..Int.MAX_VALUE, zRange: IntRange = Int.MIN_VALUE..Int.MAX_VALUE): Long {
    val cuboids = mutableListOf<Cuboid>()

    for (step in steps) {
        val newCuboids = mutableListOf<Cuboid>()
        for (cuboid in cuboids) {
            newCuboids.addAll(subtractCuboid(cuboid, step))
        }
        if (step.state) {
            newCuboids.add(step)
        }
        cuboids.clear()
        cuboids.addAll(newCuboids)
    }

    var volume = 0L
    for (cuboid in cuboids) {
        val overlapX = maxOf(cuboid.xRange.first, xRange.first)..minOf(cuboid.xRange.last, xRange.last)
        val overlapY = maxOf(cuboid.yRange.first, yRange.first)..minOf(cuboid.yRange.last, yRange.last)
        val overlapZ = maxOf(cuboid.zRange.first, zRange.first)..minOf(cuboid.zRange.last, zRange.last)
        if (overlapX.first <= overlapX.last && overlapY.first <= overlapY.last && overlapZ.first <= overlapZ.last) {
            volume += (overlapX.last - overlapX.first + 1L) * (overlapY.last - overlapY.first + 1L) * (overlapZ.last - overlapZ.first + 1L)
        }
    }

    return volume
}

fun subtractCuboid(c1: Cuboid, c2: Cuboid): List<Cuboid> {
    if (!cuboidsOverlap(c1, c2)) {
        return listOf(c1)
    }

    val result = mutableListOf<Cuboid>()

    // Split c1 into smaller cuboids that do not overlap with c2
    if (c1.xRange.first < c2.xRange.first) {
        result.add(Cuboid(true, c1.xRange.first until c2.xRange.first, c1.yRange, c1.zRange))
    }
    if (c1.xRange.last > c2.xRange.last) {
        result.add(Cuboid(true, (c2.xRange.last + 1)..c1.xRange.last, c1.yRange, c1.zRange))
    }
    if (c1.yRange.first < c2.yRange.first) {
        result.add(Cuboid(true, maxOf(c1.xRange.first, c2.xRange.first)..minOf(c1.xRange.last, c2.xRange.last), c1.yRange.first until c2.yRange.first, c1.zRange))
    }
    if (c1.yRange.last > c2.yRange.last) {
        result.add(Cuboid(true, maxOf(c1.xRange.first, c2.xRange.first)..minOf(c1.xRange.last, c2.xRange.last), (c2.yRange.last + 1)..c1.yRange.last, c1.zRange))
    }
    if (c1.zRange.first < c2.zRange.first) {
        result.add(Cuboid(true, maxOf(c1.xRange.first, c2.xRange.first)..minOf(c1.xRange.last, c2.xRange.last), maxOf(c1.yRange.first, c2.yRange.first)..minOf(c1.yRange.last, c2.yRange.last), c1.zRange.first until c2.zRange.first))
    }
    if (c1.zRange.last > c2.zRange.last) {
        result.add(Cuboid(true, maxOf(c1.xRange.first, c2.xRange.first)..minOf(c1.xRange.last, c2.xRange.last), maxOf(c1.yRange.first, c2.yRange.first)..minOf(c1.yRange.last, c2.yRange.last), (c2.zRange.last + 1)..c1.zRange.last))
    }

    return result
}

fun cuboidsOverlap(c1: Cuboid, c2: Cuboid): Boolean {
    return c1.xRange.first <= c2.xRange.last && c1.xRange.last >= c2.xRange.first &&
           c1.yRange.first <= c2.yRange.last && c1.yRange.last >= c2.yRange.first &&
           c1.zRange.first <= c2.zRange.last && c1.zRange.last >= c2.zRange.first
}