import java.io.File

data class Vec3(var x: Int, var y: Int, var z: Int)

data class Moon(var pos: Vec3, var vel: Vec3)

fun abs(x: Int): Int {
    return if (x < 0) -x else x
}

fun applyGravity(moons: MutableList<Moon>) {
    for (i in moons.indices) {
        for (j in i + 1 until moons.size) {
            if (moons[i].pos.x > moons[j].pos.x) {
                moons[i].vel.x--
                moons[j].vel.x++
            } else if (moons[i].pos.x < moons[j].pos.x) {
                moons[i].vel.x++
                moons[j].vel.x--
            }

            if (moons[i].pos.y > moons[j].pos.y) {
                moons[i].vel.y--
                moons[j].vel.y++
            } else if (moons[i].pos.y < moons[j].pos.y) {
                moons[i].vel.y++
                moons[j].vel.y--
            }

            if (moons[i].pos.z > moons[j].pos.z) {
                moons[i].vel.z--
                moons[j].vel.z++
            } else if (moons[i].pos.z < moons[j].pos.z) {
                moons[i].vel.z++
                moons[j].vel.z--
            }
        }
    }
}

fun applyVelocity(moons: MutableList<Moon>) {
    for (moon in moons) {
        moon.pos.x += moon.vel.x
        moon.pos.y += moon.vel.y
        moon.pos.z += moon.vel.z
    }
}

fun totalEnergy(moons: MutableList<Moon>): Int {
    var total = 0
    for (m in moons) {
        val pot = abs(m.pos.x) + abs(m.pos.y) + abs(m.pos.z)
        val kin = abs(m.vel.x) + abs(m.vel.y) + abs(m.vel.z)
        total += pot * kin
    }
    return total
}

fun main(args: Array<String>) {
    val file = File("input.txt")
    val moons = mutableListOf<Moon>()
    file.forEachLine {
        val regex = Regex("""<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""")
        val matchResult = regex.find(it)
        if (matchResult != null) {
            val (x, y, z) = matchResult.destructured
            moons.add(Moon(Vec3(x.toInt(), y.toInt(), z.toInt()), Vec3(0, 0, 0)))
        }
    }

    repeat(1000) {
        applyGravity(moons)
        applyVelocity(moons)
    }

    println(totalEnergy(moons))
}