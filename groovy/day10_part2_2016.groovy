def file = new File("input.txt")
def bots = [:]
def outputs = [:]

file.eachLine { line ->
    if (line =~ /value (\d+) goes to (bot \d+)/) {
        def match = (line =~ /value (\d+) goes to (bot \d+)/)
        def value = Integer.parseInt(match[0][1])
        def botID = match[0][2]
        if (!bots[botID]) bots[botID] = [chips: []]
        bots[botID].chips << value
    } else if (line =~ /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/) {
        def match = (line =~ /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/)
        def botID = match[0][1]
        def lowTo = match[0][2]
        def highTo = match[0][3]
        if (!bots[botID]) bots[botID] = [lowTo: lowTo, highTo: highTo, chips: []]
        else {
            bots[botID].lowTo = lowTo
            bots[botID].highTo = highTo
        }
    }
}

while (true) {
    def action = false
    bots.each { botID, bot ->
        if (bot.chips.size() == 2) {
            action = true
            def (low, high) = bot.chips.sort()
            bot.chips = []
            giveChip(bots, outputs, bot.lowTo, low)
            giveChip(bots, outputs, bot.highTo, high)
        }
    }
    if (!action) break
}

def result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
println result

def giveChip(bots, outputs, target, value) {
    if (target.startsWith("bot")) {
        if (!bots[target]) bots[target] = [chips: []]
        bots[target].chips << value
    } else if (target.startsWith("output")) {
        outputs[target] = value
    }
}