import strutils, tables

type
  Bot = ref object
    chips: seq[int]
    lowTo: string
    highTo: string

var
  bots = initTable[string, Bot]()
  instructions: seq[string] = @[]

for line in lines("input.txt"):
  if line.startsWith("value"):
    let parts = line.split(" ")
    let value = parseInt(parts[1])
    let botId = parts[5]
    if not bots.hasKey(botId):
      bots[botId] = Bot(chips: @[])
    bots[botId].chips.add(value)
  else:
    instructions.add(line)

for instruction in instructions:
  let parts = instruction.split(" ")
  let botId = parts[1]
  let lowTo = parts[5] & " " & parts[6]
  let highTo = parts[10] & " " & parts[11]
  if not bots.hasKey(botId):
    bots[botId] = Bot(chips: @[])
  bots[botId].lowTo = lowTo
  bots[botId].highTo = highTo

while true:
  for botId, bot in bots.pairs:
    if bot.chips.len == 2:
      let low = min(bot.chips[0], bot.chips[1])
      let high = max(bot.chips[0], bot.chips[1])
      if low == 17 and high == 61:
        echo "Bot responsible: ", botId
        quit()
      let lowParts = bot.lowTo.split(" ")
      let highParts = bot.highTo.split(" ")
      if lowParts[0] == "bot":
        if not bots.hasKey(lowParts[1]):
          bots[lowParts[1]] = Bot(chips: @[])
        bots[lowParts[1]].chips.add(low)
      if highParts[0] == "bot":
        if not bots.hasKey(highParts[1]):
          bots[highParts[1]] = Bot(chips: @[])
        bots[highParts[1]].chips.add(high)
      bot.chips = @[]