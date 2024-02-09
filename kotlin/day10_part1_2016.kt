import java.io.File

data class Bot(var lowTo: String, var highTo: String, var chips: MutableList<Int>)

fun main(args: Array<String>) {
    val bots = mutableMapOf<String, Bot>()
    val valueRegex = Regex("""value (\d+) goes to (bot \d+)""")
    val givesRegex = Regex("""(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)""")

    File("input.txt").forEachLine { line ->
        if (valueRegex.matches(line)) {
            val (value, botID) = valueRegex.find(line)!!.destructured
            val chipValue = value.toInt()

            if (!bots.containsKey(botID)) {
                bots[botID] = Bot("", "", mutableListOf())
            }
            bots[botID]!!.chips.add(chipValue)

        } else if (givesRegex.matches(line)) {
            val (botID, lowTo, highTo) = givesRegex.find(line)!!.destructured

            if (!bots.containsKey(botID)) {
                bots[botID] = Bot("", "", mutableListOf())
            }
            bots[botID]!!.lowTo = lowTo
            bots[botID]!!.highTo = highTo
        }
    }

    while (true) {
        var action = false
        for ((botID, b) in bots) {
            if (b.chips.size == 2) {
                action = true
                val (low, high) = minMax(b.chips[0], b.chips[1])
                if (low == 17 && high == 61) {
                    println(botID)
                    return
                }
                b.chips.clear()

                giveChip(bots, b.lowTo, low)
                giveChip(bots, b.highTo, high)
            }
        }
        if (!action) {
            break
        }
    }
}

fun giveChip(bots: MutableMap<String, Bot>, target: String, value: Int) {
    if (!bots.containsKey(target)) {
        bots[target] = Bot("", "", mutableListOf())
    }
    bots[target]!!.chips.add(value)
}

fun minMax(a: Int, b: Int): Pair<Int, Int> {
    return if (a < b) Pair(a, b) else Pair(b, a)
}