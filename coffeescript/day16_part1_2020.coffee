fs = require 'fs'

class Rule
  constructor: (@name, @ranges) ->

  isValid: (value) ->
    for rng in @ranges
      return true if value >= rng[0] and value <= rng[1]
    false

toInt = (s) -> parseInt(s, 10)

isValidForAnyRule = (value, rules) ->
  for rule in rules
    return true if rule.isValid(value)
  false

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  lines = data.split '\n'
  rules = []
  scanningRules = true
  errorRate = 0

  reRule = /^(.+): (\d+)-(\d+) or (\d+)-(\d+)$/

  for line in lines
    continue if line == ""

    if line == "your ticket:" or line == "nearby tickets:"
      scanningRules = false
      continue

    if scanningRules
      matches = reRule.exec line
      if matches
        name = matches[1]
        range1 = [toInt(matches[2]), toInt(matches[3])]
        range2 = [toInt(matches[4]), toInt(matches[5])]
        rules.push new Rule(name, [range1, range2])
    else
      values = line.split ','
      for value in values
        val = toInt(value)
        errorRate += val unless isValidForAnyRule(val, rules)

  console.log errorRate