
import java.io.File

fun main() {
    val lines = File("input.txt").readLines()
    val initialState = lines[0].substringAfter("Begin in state ").substringBefore(".")
    val steps = lines[1].substringAfter("Perform a diagnostic checksum after ").substringBefore(" steps").toInt()

    val states = mutableMapOf<String, State>()
    var i = 3
    while (i < lines.size) {
        val stateName = lines[i].substringAfter("In state ").substringBefore(":")
        val zeroWrite = lines[i + 2].substringAfter("Write the value ").substringBefore(".").toInt()
        val zeroMove = if (lines[i + 3].contains("right")) 1 else -1
        val zeroNextState = lines[i + 4].substringAfter("Continue with state ").substringBefore(".")
        val oneWrite = lines[i + 6].substringAfter("Write the value ").substringBefore(".").toInt()
        val oneMove = if (lines[i + 7].contains("right")) 1 else -1
        val oneNextState = lines[i + 8].substringAfter("Continue with state ").substringBefore(".")
        states[stateName] = State(
            Rule(zeroWrite, zeroMove, zeroNextState),
            Rule(oneWrite, oneMove, oneNextState)
        )
        i += 10
    }

    val tape = mutableMapOf<Int, Int>().withDefault { 0 }
    var cursor = 0
    var currentState = initialState

    repeat(steps) {
        val currentValue = tape.getValue(cursor)
        val rule = if (currentValue == 0) states[currentState]!!.zeroRule else states[currentState]!!.oneRule
        tape[cursor] = rule.writeValue
        cursor += rule.move
        currentState = rule.nextState
    }

    println(tape.values.count { it == 1 })
}

data class State(val zeroRule: Rule, val oneRule: Rule)
data class Rule(val writeValue: Int, val move: Int, val nextState: String)
