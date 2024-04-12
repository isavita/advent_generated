fs = require 'fs'

countBags = (color, rules) ->
  count = 1
  count += rule.Count * countBags(rule.Color, rules) for rule in rules[color] ? []
  count

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  rules = {}
  ruleRegex = /(\d+) (\w+ \w+) bags?[,.]/g

  for line in data.trim().split('\n')
    [container, contents] = line.split ' bags contain '
    continue if contents == "no other bags."
    while match = ruleRegex.exec(contents)
      [_, count, color] = match
      count = parseInt count
      rules[container] = (rules[container] ? []).concat {Color: color, Count: count}

  totalBags = countBags('shiny gold', rules) - 1
  console.log totalBags