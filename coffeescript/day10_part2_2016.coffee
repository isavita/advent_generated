fs = require 'fs'

class Bot
  constructor: ->
    @lowTo = null
    @highTo = null
    @chips = []

giveChip = (bots, outputs, target, value) ->
  if target.startsWith 'bot'
    bots[target] ?= new Bot()
    bots[target].chips.push value
  else if target.startsWith 'output'
    outputs[target] = value

minMax = (a, b) ->
  if a < b then [a, b] else [b, a]

data = fs.readFileSync 'input.txt', 'utf-8'
bots = {}
outputs = {}
valueRegex = /value (\d+) goes to (bot \d+)/
givesRegex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/

for line in data.trim().split '\n'
  if valueMatch = valueRegex.exec line
    [_, value, botID] = valueMatch
    value = parseInt value
    bots[botID] ?= new Bot()
    bots[botID].chips.push value
  else if givesMatch = givesRegex.exec line
    [_, botID, lowTo, highTo] = givesMatch
    bots[botID] ?= new Bot()
    bots[botID].lowTo = lowTo
    bots[botID].highTo = highTo

action = true
while action
  action = false
  for botID, bot of bots
    if bot.chips.length == 2
      action = true
      [low, high] = minMax(bot.chips[0], bot.chips[1])
      bot.chips = []
      giveChip bots, outputs, bot.lowTo, low
      giveChip bots, outputs, bot.highTo, high

result = outputs['output 0'] * outputs['output 1'] * outputs['output 2']
console.log result