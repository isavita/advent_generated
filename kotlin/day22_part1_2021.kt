import java.io.File

fun main() {
    val input = File("input.txt").readLines()

    val rebootSteps = input.map { line ->
        val (state, ranges) = line.split(' ')
        val (x, y, z) = ranges.split(',').map { it.split('=')[1].split("..").map { it.toInt() } }
        RebootStep(state == "on", x[0]..x[1], y[0]..y[1], z[0]..z[1])
    }

    val reactor = Reactor()
    rebootSteps.forEach { step ->
        if (step.x.first >= -50 && step.x.last <= 50 &&
            step.y.first >= -50 && step.y.last <= 50 &&
            step.z.first >= -50 && step.z.last <= 50
        ) {
            reactor.applyRebootStep(step)
        }
    }

    val result = reactor.cubesOn()
    println("The final answer is $result.")
}

data class RebootStep(val on: Boolean, val x: IntRange, val y: IntRange, val z: IntRange)

class Reactor {
    private val cubes = mutableMapOf<Cube, Boolean>()

    fun applyRebootStep(step: RebootStep) {
        for (x in step.x) {
            for (y in step.y) {
                for (z in step.z) {
                    cubes[Cube(x, y, z)] = step.on
                }
            }
        }
    }

    fun cubesOn(): Int {
        return cubes.values.count { it }
    }
}

data class Cube(val x: Int, val y: Int, val z: Int)