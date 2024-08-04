import java.io.File
import kotlin.math.abs

data class Vector3D(val x: Int, val y: Int, val z: Int) {
    operator fun plus(other: Vector3D) = Vector3D(x + other.x, y + other.y, z + other.z)
    operator fun minus(other: Vector3D) = Vector3D(x - other.x, y - other.y, z - other.z)
    fun absSum() = abs(x) + abs(y) + abs(z)
}

data class Moon(var position: Vector3D, var velocity: Vector3D) {
    fun potentialEnergy() = position.absSum()
    fun kineticEnergy() = velocity.absSum()
    fun totalEnergy() = potentialEnergy() * kineticEnergy()
}

fun applyGravity(moons: List<Moon>) {
    for (i in 0 until moons.size) {
        for (j in i + 1 until moons.size) {
            val (m1, m2) = moons[i].position to moons[j].position
            val dx = if (m1.x < m2.x) 1 else if (m1.x > m2.x) -1 else 0
            val dy = if (m1.y < m2.y) 1 else if (m1.y > m2.y) -1 else 0
            val dz = if (m1.z < m2.z) 1 else if (m1.z > m2.z) -1 else 0
            moons[i].velocity += Vector3D(dx, dy, dz)
            moons[j].velocity += Vector3D(-dx, -dy, -dz)
        }
    }
}

fun applyVelocity(moons: List<Moon>) {
    moons.forEach { it.position += it.velocity }
}

fun simulate(moons: List<Moon>, steps: Int): List<Moon> {
    repeat(steps) {
        applyGravity(moons)
        applyVelocity(moons)
    }
    return moons
}

fun calculateTotalEnergy(moons: List<Moon>): Int {
    return moons.sumOf { it.totalEnergy() }
}

fun findRepeatState(moons: List<Moon>): Long {
    val initialState = moons.map { it.position to it.velocity }
    var step = 0L
    var xRepeat = 0L
    var yRepeat = 0L
    var zRepeat = 0L

    while (true) {
        applyGravity(moons)
        applyVelocity(moons)
        step++

        if (xRepeat == 0L && moons.all { it.position.x == initialState[moons.indexOf(it)].first.x && it.velocity.x == initialState[moons.indexOf(it)].second.x }) {
            xRepeat = step
        }
        if (yRepeat == 0L && moons.all { it.position.y == initialState[moons.indexOf(it)].first.y && it.velocity.y == initialState[moons.indexOf(it)].second.y }) {
            yRepeat = step
        }
        if (zRepeat == 0L && moons.all { it.position.z == initialState[moons.indexOf(it)].first.z && it.velocity.z == initialState[moons.indexOf(it)].second.z }) {
            zRepeat = step
        }

        if (xRepeat != 0L && yRepeat != 0L && zRepeat != 0L) {
            return lcm(lcm(xRepeat, yRepeat), zRepeat)
        }
    }
}

fun gcd(a: Long, b: Long): Long {
    return if (b == 0L) a else gcd(b, a % b)
}

fun lcm(a: Long, b: Long): Long {
    return a * (b / gcd(a, b))
}

fun main() {
    val input = File("input.txt").readLines()
    val moons = input.map { line ->
        val (x, y, z) = Regex("""<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""").find(line)!!.destructured
        Moon(Vector3D(x.toInt(), y.toInt(), z.toInt()), Vector3D(0, 0, 0))
    }

    val simulatedMoons = simulate(moons.map { it.copy() }, 1000)
    val totalEnergy = calculateTotalEnergy(simulatedMoons)
    println("Total energy after 1000 steps: $totalEnergy")

    val repeatSteps = findRepeatState(moons)
    println("Number of steps to repeat the initial state: $repeatSteps")
}