import sequtils, strutils, tables

proc readInput(filename: string): (string, Table[string, char]) =
  let file = readFile(filename)
  let lines = file.splitLines()
  let tmpl = lines[0]
  var rules = initTable[string, char]()
  for line in lines[2..^1]:
    let parts = line.split(" -> ")
    rules[parts[0]] = parts[1][0]
  (tmpl, rules)

proc applyInsertion(tmpl: string, rules: Table[string, char]): string =
  var result = newSeq[char]()
  for i in 0..<tmpl.len - 1:
    result.add(tmpl[i])
    let pair = tmpl[i..i+1]
    if rules.hasKey(pair):
      result.add(rules[pair])
  result.add(tmpl[^1])
  result.join("")

proc countElements(polymer: string): Table[char, int] =
  var counts = initTable[char, int]()
  for char in polymer:
    if counts.hasKey(char):
      counts[char] += 1
    else:
      counts[char] = 1
  counts

proc findMostAndLeastCommon(counts: Table[char, int]): (int, int) =
  var mostCommon = 0
  var leastCommon = high(int)
  for count in counts.values:
    if count > mostCommon:
      mostCommon = count
    if count < leastCommon:
      leastCommon = count
  (mostCommon, leastCommon)

let (tmpl, rules) = readInput("input.txt")
var polymer = tmpl
for _ in 1..10:
  polymer = applyInsertion(polymer, rules)
let counts = countElements(polymer)
let (mostCommon, leastCommon) = findMostAndLeastCommon(counts)
echo mostCommon - leastCommon