
def minMax(a, b) { a < b ? [a, b] : [b, a] }

def bots = [:]
def valueRegex = ~'value (\\d+) goes to (bot \\d+)'
def givesRegex = ~'(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)'

new File("input.txt").eachLine { line ->
    def matcher = valueRegex.matcher(line)
    if (matcher.matches()) {
        def value = matcher[0][1] as Integer
        def botId = matcher[0][2]
        bots[botId] = bots.getOrDefault(botId, [lowTo: "", highTo: "", chips: []])
        bots[botId].chips << value
    } else {
        matcher = givesRegex.matcher(line)
        if (matcher.matches()) {
            def botId = matcher[0][1]
            def lowTo = matcher[0][2]
            def highTo = matcher[0][3]
            bots[botId] = bots.getOrDefault(botId, [lowTo: "", highTo: "", chips: []])
            bots[botId].lowTo = lowTo
            bots[botId].highTo = highTo
        }
    }
}

while (true) {
    def action = false
    bots.each { botId, b ->
        if (b.chips.size() == 2) {
            action = true
            def (low, high) = minMax(b.chips[0], b.chips[1])
            if (low == 17 && high == 61) {
                println botId
                System.exit(0)
            }
            b.chips = []

            if (b.lowTo) {
                bots[b.lowTo] = bots.getOrDefault(b.lowTo, [lowTo: "", highTo: "", chips: []])
                bots[b.lowTo].chips << low
            }
            if (b.highTo) {
                bots[b.highTo] = bots.getOrDefault(b.highTo, [lowTo: "", highTo: "", chips: []])
                bots[b.highTo].chips << high
            }
        }
    }
    if (!action) break
}
