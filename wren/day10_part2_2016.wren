
import "io" for File

class Instruction {
    construct new(bot, lowTarget, lowId, highTarget, highId) {
        _bot = bot
        _lowTarget = lowTarget
        _lowId = lowId
        _highTarget = highTarget
        _highId = highId
    }
    bot          { _bot }
    lowTarget    { _lowTarget }
    lowId        { _lowId }
    highTarget   { _highTarget }
    highId       { _highId }
}

var lines = File.read("input.txt").split("\n")
var bots = {}
var outputs = {}
var botInstr = {}

for (line in lines) {
    if (line == "") continue
    var p = line.split(" ")
    if (p[0] == "value") {
        var value = Num.fromString(p[1])
        var botId = Num.fromString(p[5])
        if (!bots.containsKey(botId)) bots[botId] = []
        bots[botId].add(value)
    } else {
        var botId = Num.fromString(p[1])
        var lowTarget = p[5]
        var lowId = Num.fromString(p[6])
        var highTarget = p[10]
        var highId = Num.fromString(p[11])
        botInstr[botId] = Instruction.new(botId, lowTarget, lowId, highTarget, highId)
    }
}

var responsibleBot = -1
var changed = true
while (changed) {
    changed = false
    for (bid in bots.keys) {
        var chips = bots[bid]
        if (chips.count != 2) continue
        changed = true

        var a = chips[0]
        var b = chips[1]
        var lo = a.min(b)
        var hi = a.max(b)

        if (lo == 17 && hi == 61) responsibleBot = bid

        var instr = botInstr[bid]

        if (instr.lowTarget == "bot") {
            if (!bots.containsKey(instr.lowId)) bots[instr.lowId] = []
            bots[instr.lowId].add(lo)
        } else {
            outputs[instr.lowId] = lo
        }

        if (instr.highTarget == "bot") {
            if (!bots.containsKey(instr.highId)) bots[instr.highId] = []
            bots[instr.highId].add(hi)
        } else {
            outputs[instr.highId] = hi
        }

        bots[bid] = []
        break   // restart scanning after state change
    }
}

var product = outputs[0] * outputs[1] * outputs[2]
System.print("Responsible bot: %(responsibleBot)")
System.print("Output product:   %(product)")
