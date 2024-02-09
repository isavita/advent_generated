import java.io.File

data class Particle(var p: Array<Int>, var v: Array<Int>, var a: Array<Int>)

fun main(args: Array<String>) {
    val file = File("input.txt")
    val particles = mutableListOf<Particle>()

    file.forEachLine {
        val parts = it.split(", ")

        val p = Particle(Array(3) { 0 }, Array(3) { 0 }, Array(3) { 0 })
        for (i in parts.indices) {
            val coords = parts[i].substring(3, parts[i].length - 1).split(",")
            for (j in coords.indices) {
                val num = coords[j].toInt()
                when (i) {
                    0 -> p.p[j] = num
                    1 -> p.v[j] = num
                    2 -> p.a[j] = num
                }
            }
        }
        particles.add(p)
    }

    repeat(1000) {
        val positions = mutableMapOf<String, Int>()
        particles.forEachIndexed { index, particle ->
            repeat(3) { j ->
                particle.v[j] += particle.a[j]
                particle.p[j] += particle.v[j]
            }
            particles[index] = particle
            val posStr = "${particle.p[0]},${particle.p[1]},${particle.p[2]}"
            positions[posStr] = positions.getOrDefault(posStr, 0) + 1
        }

        val newParticles = particles.filter { particle ->
            val posStr = "${particle.p[0]},${particle.p[1]},${particle.p[2]}"
            positions[posStr] == 1
        }
        particles.clear()
        particles.addAll(newParticles)
    }

    println(particles.size)
}