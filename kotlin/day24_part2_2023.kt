
import java.io.File
import java.math.BigDecimal
import java.math.BigInteger
import java.math.RoundingMode

data class RatVec3(
    var x: BigDecimal,
    var y: BigDecimal,
    var z: BigDecimal
) {
    operator fun plus(other: RatVec3): RatVec3 {
        return RatVec3(x + other.x, y + other.y, z + other.z)
    }

    operator fun minus(other: RatVec3): RatVec3 {
        return RatVec3(x - other.x, y - other.y, z - other.z)
    }

    operator fun times(s: BigDecimal): RatVec3 {
        return RatVec3(x * s, y * s, z * s)
    }

    operator fun div(s: BigDecimal): RatVec3 {
        return RatVec3(x / s, y / s, z / s)
    }

    fun cross(other: RatVec3): RatVec3 {
        return RatVec3(
            y * other.z - z * other.y,
            z * other.x - x * other.z,
            x * other.y - y * other.x
        )
    }

    fun dot(other: RatVec3): BigDecimal {
        return x * other.x + y * other.y + z * other.z
    }
}

data class HailStone(
    val p: RatVec3,
    val v: RatVec3
) {
    operator fun minus(other: HailStone): HailStone {
        return HailStone(p - other.p, v - other.v)
    }
}

fun main() {
    val input = File("input.txt").readLines()
    println(solve(input))
}

fun solve(input: List<String>): String {
    val hailStones = readInput(input.take(3))
    val s1 = hailStones[1]
    val s2 = hailStones[2]
    val ref1 = s1 - hailStones[0]
    val ref2 = s2 - hailStones[0]

    val t1 = intersectionTime(ref2, ref1)
    val t2 = intersectionTime(ref1, ref2)

    val rock1 = s1.p + s1.v * t1
    val rock2 = s2.p + s2.v * t2

    val rp = rock1 - (rock2 - rock1) / (t2 - t1) * t1
    return (rp.x + rp.y + rp.z).setScale(0, RoundingMode.HALF_UP).toPlainString()
}

fun readInput(input: List<String>): List<HailStone> {
    return input.map { readLine(it) }
}

fun readLine(line: String): HailStone {
    val match = Regex("-?\\d+").findAll(line).map { it.value.toBigInteger() }.toList()
    return HailStone(
        p = RatVec3(
            match[0].toBigDecimal(),
            match[1].toBigDecimal(),
            match[2].toBigDecimal()
        ),
        v = RatVec3(
            match[3].toBigDecimal(),
            match[4].toBigDecimal(),
            match[5].toBigDecimal()
        )
    )
}

fun intersectionTime(r: HailStone, s: HailStone): BigDecimal {
    val plane = r.p.cross(r.p + r.v)
    return (s.p.dot(plane) * BigDecimal.valueOf(-1)) / s.v.dot(plane)
}
