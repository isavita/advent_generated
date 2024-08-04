fs = require 'fs'

class Rule
  constructor: (@name, @ranges) ->

  isValid: (value) ->
    for range in @ranges
      [low, high] = range
      return true if value >= low and value <= high
    false

toInt = (s) -> parseInt s, 10

parseTicket = (s) ->
  s.split(',').map toInt

isValidTicket = (ticket, rules) ->
  for value in ticket
    return false unless isValidForAnyRule value, rules
  true

isValidForAnyRule = (value, rules) ->
  for rule in rules
    return true if rule.isValid value
  false

solveFieldPositions = (rules, tickets) ->
  validPositions = {}
  for rule in rules
    validPositions[rule.name] = {}
    for i in [0...tickets[0].length]
      valid = true
      for ticket in tickets
        valid = false if not rule.isValid ticket[i]
        break unless valid
      validPositions[rule.name][i] = true if valid

  fieldPositions = {}
  while Object.keys(fieldPositions).length < rules.length
    for name, positions of validPositions
      if Object.keys(positions).length == 1
        for pos of positions
          fieldPositions[name] = parseInt pos
          for otherName of validPositions
            delete validPositions[otherName][pos]
        delete validPositions[name]

  fieldPositions

calculateDepartureProduct = (ticket, fieldPositions) ->
  product = 1
  for name, pos of fieldPositions
    product *= ticket[pos] if name.startsWith 'departure'
  product

fileContent = fs.readFileSync 'input.txt', 'utf8'
lines = fileContent.split '\n'
rules = []
myTicket = []
nearbyTickets = []
section = 0
reRule = /^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/

for line in lines
  if line == ''
    section++
    continue

  switch section
    when 0
      match = reRule.exec line
      if match
        rules.push new Rule match[1], [
          [toInt(match[2]), toInt(match[3])]
          [toInt(match[4]), toInt(match[5])]
        ]
    when 1
      if line != 'your ticket:'
        myTicket = parseTicket line
    when 2
      if line != 'nearby tickets:'
        ticket = parseTicket line
        nearbyTickets.push ticket if isValidTicket ticket, rules

fieldPositions = solveFieldPositions rules, nearbyTickets
departureProduct = calculateDepartureProduct myTicket, fieldPositions

console.log departureProduct