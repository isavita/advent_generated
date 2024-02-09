import java.io.File

fun main(args: Array<String>) {
    val bots = mutableMapOf<String, Bot>()
    val outputs = mutableMapOf<String, Int>()
    val valueRegex = Regex("""value (\d+) goes to (bot \d+)""")
    val givesRegex = Regex("""(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)""")

    File("input.txt").forEachLine { line ->
        if (valueRegex.containsMatchIn(line)) {
            val (value, botID) = valueRegex.find(line)!!.destructured
            bots.getOrPut(botID) { Bot() }.chips.add(value.toInt())
        } else if (givesRegex.containsMatchIn(line)) {
            val (botID, lowTo, highTo) = givesRegex.find(line)!!.destructured
            bots.getOrPut(botID) { Bot() }.apply {
                this.lowTo = lowTo
                this.highTo = highTo
            }
        }
    }

    while (true) {
        var action = false
        for (b in bots.values) {
            if (b.chips.size == 2) {
                action = true
                val (low, high) = minMax(b.chips[0], b.chips[1])
                b.chips.clear()

                giveChip(bots, outputs, b.lowTo, low)
                giveChip(bots, outputs, b.highTo, high)
            }
        }
        if (!action) {
            break
        }
    }

    val result = outputs["output 0"]!! * outputs["output 1"]!! * outputs["output 2"]!!
    println(result)
}

fun giveChip(bots: MutableMap<String, Bot>, outputs: MutableMap<String, Int>, target: String, value: Int) {
    if (target.startsWith("bot")) {
        bots.getOrPut(target) { Bot() }.chips.add(value)
    } else if (target.startsWith("output")) {
        outputs[target] = value
    }
}

fun minMax(a: Int, b: Int): Pair<Int, Int> {
    return if (a < b) a to b else b to a
}

class Bot {
    var lowTo: String = ""
    var highTo: String = ""
    val chips = mutableListOf<Int>()
}