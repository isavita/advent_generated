fs = require 'fs'

class Bot
  constructor: (@lowTo, @highTo, @chips = []) ->

bots = {}
valueRegex = /value (\d+) goes to (bot \d+)/
givesRegex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err
  lines = data.split '\n'
  for line in lines
    if valueRegex.test line
      [_, value, botID] = valueRegex.exec line
      value = parseInt value
      bots[botID] ?= new Bot()
      bots[botID].chips.push value
    else if givesRegex.test line
      [_, botID, lowTo, highTo] = givesRegex.exec line
      bots[botID] ?= new Bot()
      bots[botID].lowTo = lowTo
      bots[botID].highTo = highTo

  loop
    action = false
    for botID, b of bots
      if b.chips.length == 2
        action = true
        [low, high] = [Math.min(b.chips...), Math.max(b.chips...)]
        if low == 17 and high == 61
          console.log botID
          return
        b.chips = []
        giveChip bots, b.lowTo, low
        giveChip bots, b.highTo, high
    break unless action

giveChip = (bots, target, value) ->
  bots[target] ?= new Bot()
  bots[target].chips.push value