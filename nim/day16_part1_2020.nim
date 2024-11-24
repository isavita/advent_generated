
import std/[strutils, strscans, sequtils, streams]

type
  Rule = object
    name: string
    ranges: seq[array[2, int]]

proc isValid(r: Rule, value: int): bool =
  for rng in r.ranges:
    if value >= rng[0] and value <= rng[1]:
      return true
  false

proc toInt(s: string): int =
  parseInt(s)

proc isValidForAnyRule(value: int, rules: seq[Rule]): bool =
  for rule in rules:
    if rule.isValid(value):
      return true
  false

proc main() =
  var 
    file = newFileStream("input.txt", fmRead)
    line = ""
    rules: seq[Rule] = @[]
    scanningRules = true
    errorRate = 0

  if file == nil:
    quit "Cannot open input file"

  while file.readLine(line):
    if line.len == 0:
      continue

    if line.startsWith("your ticket:") or line.startsWith("nearby tickets:"):
      scanningRules = false
      continue

    if scanningRules:
      var name: string
      var a, b, c, d: int
      if scanf(line, "$+: $i-$i or $i-$i", name, a, b, c, d):
        rules.add(Rule(name: name, ranges: @[[a, b], [c, d]]))
    else:
      for value in line.split(","):
        let val = value.toInt
        if not isValidForAnyRule(val, rules):
          errorRate += val

  echo errorRate
  file.close()

main()
