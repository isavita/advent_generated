import strutils, sequtils, tables

type
  BagRule = object
    color: string
    count: int

  BagRules = Table[string, seq[BagRule]]

proc parseBagRule(line: string): (string, seq[BagRule]) =
  let parts = line.split(" bags contain ")
  let color = parts[0]
  let contents = parts[1].split(", ")
  var rules: seq[BagRule] = @[]
  for content in contents:
    if content == "no other bags.":
      continue
    let parts = content.split(" ")
    let count = parseInt(parts[0])
    let color = parts[1] & " " & parts[2]
    rules.add(BagRule(color: color, count: count))
  (color, rules)

proc countBags(rules: BagRules, color: string): int =
  var count = 0
  for rule in rules[color]:
    count += rule.count + rule.count * countBags(rules, rule.color)
  count

when isMainModule:
  let input = readFile("input.txt").splitLines()
  var rules: BagRules = initTable[string, seq[BagRule]]()
  for line in input:
    let (color, rule) = parseBagRule(line)
    rules[color] = rule
  echo countBags(rules, "shiny gold")