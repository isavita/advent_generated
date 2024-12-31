
import strutils, sequtils, tables

type
  Target = enum
    Bot
    Output

  Instruction = object
    bot: int
    lowTarget: Target
    lowTargetId: int
    highTarget: Target
    highTargetId: int

  ValueInstruction = object
    value: int
    bot: int

proc parseInstruction(line: string): Instruction =
  let parts = line.split(" ")
  if parts[0] == "bot":
    let bot = parseInt(parts[1])
    let lowTarget = if parts[5] == "bot": Target.Bot else: Target.Output
    let lowTargetId = parseInt(parts[6])
    let highTarget = if parts[10] == "bot": Target.Bot else: Target.Output
    let highTargetId = parseInt(parts[11])
    return Instruction(bot: bot, lowTarget: lowTarget, lowTargetId: lowTargetId, highTarget: highTarget, highTargetId: highTargetId)
  else:
    raise newException(ValueError, "Invalid instruction format")

proc parseValueInstruction(line: string): ValueInstruction =
  let parts = line.split(" ")
  if parts[0] == "value":
    let value = parseInt(parts[1])
    let bot = parseInt(parts[5])
    return ValueInstruction(value: value, bot: bot)
  else:
    raise newException(ValueError, "Invalid value instruction format")

proc solve(instructions: seq[string]): (int, int) =
  var
    bots: Table[int, seq[int]] = initTable[int, seq[int]]()
    outputs: Table[int, int] = initTable[int, int]()
    botInstructions: Table[int, Instruction] = initTable[int, Instruction]()
    valueInstructions: seq[ValueInstruction] = @[]

  for line in instructions:
    if line.startsWith("value"):
      valueInstructions.add(parseValueInstruction(line))
    else:
      let instruction = parseInstruction(line)
      botInstructions[instruction.bot] = instruction

  for valueInstruction in valueInstructions:
    if not bots.hasKey(valueInstruction.bot):
      bots[valueInstruction.bot] = @[]
    bots[valueInstruction.bot].add(valueInstruction.value)

  var responsibleBot = -1
  while true:
    var botProcessed = false
    for bot, chips in bots.pairs:
      if chips.len == 2:
        botProcessed = true
        let lowChip = min(chips[0], chips[1])
        let highChip = max(chips[0], chips[1])

        if lowChip == 17 and highChip == 61:
          responsibleBot = bot

        let instruction = botInstructions[bot]

        if instruction.lowTarget == Target.Bot:
          if not bots.hasKey(instruction.lowTargetId):
            bots[instruction.lowTargetId] = @[]
          bots[instruction.lowTargetId].add(lowChip)
        else:
          outputs[instruction.lowTargetId] = lowChip

        if instruction.highTarget == Target.Bot:
          if not bots.hasKey(instruction.highTargetId):
            bots[instruction.highTargetId] = @[]
          bots[instruction.highTargetId].add(highChip)
        else:
          outputs[instruction.highTargetId] = highChip

        bots[bot] = @[]
        break
    if not botProcessed:
      break

  let outputProduct = outputs[0] * outputs[1] * outputs[2]
  return (responsibleBot, outputProduct)

let instructions = readFile("input.txt").splitLines()
let (responsibleBot, outputProduct) = solve(instructions)
echo "Responsible bot: ", responsibleBot
echo "Output product: ", outputProduct
