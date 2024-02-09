import java.io.File
import kotlin.math.abs

data class Particle(val p: IntArray, val v: IntArray, val a: IntArray)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val particles = mutableListOf<Particle>()

    file.forEachLine {
        val parts = it.split(", ")

        val p = IntArray(3)
        val v = IntArray(3)
        val a = IntArray(3)

        parts.forEachIndexed { i, part ->
            val coords = part.substring(3, part.length - 1).split(",")
            coords.forEachIndexed { j, coord ->
                val num = coord.toInt()
                when (i) {
                    0 -> p[j] = num
                    1 -> v[j] = num
                    2 -> a[j] = num
                }
            }
        }

        particles.add(Particle(p, v, a))
    }

    var closestParticle = 0
    var minAccel = Int.MAX_VALUE
    var minVelocity = Int.MAX_VALUE
    var minPosition = Int.MAX_VALUE

    particles.forEachIndexed { i, particle ->
        val accel = manhattan(particle.a)
        val velocity = manhattan(particle.v)
        val position = manhattan(particle.p)

        if (accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
            (accel == minAccel && velocity == minVelocity && position < minPosition)) {
            minAccel = accel
            minVelocity = velocity
            minPosition = position
            closestParticle = i
        }
    }

    println(closestParticle)
}

fun manhattan(x: IntArray): Int {
    return x.sumBy { abs(it) }
}