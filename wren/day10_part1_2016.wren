
import "io" for File

class Bot {
  construct new(id) {
    _id = id
    _chips = []
    _lowTarget = null
    _highTarget = null
    _lowType = null
    _highType = null
  }

  id { _id }
  chips { _chips }

  addChip(chip) {
    _chips.add(chip)
    if (_chips.count == 2) {
      processChips()
    }
  }

  setInstructions(lowType, lowTarget, highType, highTarget) {
    _lowType = lowType
    _lowTarget = lowTarget
    _highType = highType
    _highTarget = highTarget
  }

  processChips() {
    _chips.sort()
    var lowChip = _chips[0]
    var highChip = _chips[1]

    if (lowChip == 17 && highChip == 61) {
      System.print("Bot comparing 17 and 61: " + _id)
    }

    if (_lowType == "bot") {
      _lowTarget.addChip(lowChip)
    } else {
      _outputs[_lowTarget] = lowChip
    }

    if (_highType == "bot") {
      _highTarget.addChip(highChip)
    } else {
      _outputs[_highTarget] = highChip
    }
    _chips.clear()
  }
}

var bots = {}
var _outputs = {}

var lines = File.read("input.txt").split("\n")
for (line in lines) {
  if (line.startsWith("value")) {
    var parts = line.split(" ")
    var value = Num.fromString(parts[1])
    var botId = Num.fromString(parts[5])
    if (!bots.containsKey(botId)) {
      bots[botId] = Bot.new(botId)
    }
    bots[botId].addChip(value)
  } else if (line.startsWith("bot")) {
    var parts = line.split(" ")
    var botId = Num.fromString(parts[1])
    var lowType = parts[5]
    var lowTarget = Num.fromString(parts[6])
    var highType = parts[10]
    var highTarget = Num.fromString(parts[11])

    if (!bots.containsKey(botId)) {
      bots[botId] = Bot.new(botId)
    }

    var lowTargetBot = null
    if (lowType == "bot") {
      if (!bots.containsKey(lowTarget)) {
        bots[lowTarget] = Bot.new(lowTarget)
      }
      lowTargetBot = bots[lowTarget]
    }

    var highTargetBot = null
    if (highType == "bot") {
      if (!bots.containsKey(highTarget)) {
        bots[highTarget] = Bot.new(highTarget)
      }
      highTargetBot = bots[highTarget]
    }

    bots[botId].setInstructions(lowType, lowTargetBot ?? lowTarget, highType, highTargetBot ?? highTarget)
  }
}
