
import std/[strutils, algorithm, sequtils, tables, strformat]

type
  Gate = object
    a, op, b: string
  GateWithOutput = object
    gate: Gate
    output: string

proc parse(input: string): seq[GateWithOutput] =
  let pos = input.find("\n\n")
  if pos == -1: return
  for line in input[pos+2..^1].splitLines:
    if line == "": continue
    let parts = line.split(" ")
    if parts.len == 5 and parts[3] == "->":
      result.add GateWithOutput(gate: Gate(a: parts[0], op: parts[1], b: parts[2]), output: parts[4])

proc getReverseLookupKey(a, op, b: string): string =
  result = if a < b: a & "_" & op & "_" & b else: b & "_" & op & "_" & a

proc createLookups(gates: seq[GateWithOutput]): (Table[string, Gate], Table[string, string]) =
  var lookup: Table[string, Gate]
  var reverseLookup: Table[string, string]
  for g in gates:
    lookup[g.output] = g.gate
    reverseLookup[getReverseLookupKey(g.gate.a, g.gate.op, g.gate.b)] = g.output
  result = (lookup, reverseLookup)

proc swapWires(pairs: var seq[tuple[a, b: string]], gates: var seq[GateWithOutput], a, b: string) =
  pairs.add (a, b)
  for i in 0..<gates.len:
    if gates[i].output == a: gates[i].output = b
    elif gates[i].output == b: gates[i].output = a

proc findInMap(m: Table[string, string], key: string): string =
  m.getOrDefault(key, "")

proc padNum(i: int): string =
  result = $i
  if result.len < 2: result = "0" & result

proc solution(gates: var seq[GateWithOutput]): string =
  var pairs: seq[tuple[a, b: string]]
  var numZ = 0
  for g in gates:
    if g.output.len > 0 and g.output[0] == 'z': inc numZ
  while pairs.len < 4:
    let (lookup, reverseLookup) = createLookups(gates)
    var carryIn = ""
    var swapped = false
    for i in 0..<numZ:
      let xi = "x" & padNum(i)
      let yi = "y" & padNum(i)
      let zi = "z" & padNum(i)
      var foundAdder = ""
      var carryOut = carryIn
      if i == 0:
        foundAdder = findInMap(reverseLookup, getReverseLookupKey(xi, "XOR", yi))
        carryOut = findInMap(reverseLookup, getReverseLookupKey(xi, "AND", yi))
      else:
        let bit = findInMap(reverseLookup, getReverseLookupKey(xi, "XOR", yi))
        if bit != "" and carryIn != "":
          foundAdder = findInMap(reverseLookup, getReverseLookupKey(bit, "XOR", carryIn))
          if foundAdder != "":
            let c1 = findInMap(reverseLookup, getReverseLookupKey(xi, "AND", yi))
            let c2 = findInMap(reverseLookup, getReverseLookupKey(bit, "AND", carryIn))
            carryOut = if c1 != "" and c2 != "":
                         findInMap(reverseLookup, getReverseLookupKey(c1, "OR", c2))
                       else: ""
      if foundAdder == "":
        if carryIn != "" and lookup.hasKey(zi):
          let gate = lookup[zi]
          let bit = findInMap(reverseLookup, getReverseLookupKey(xi, "XOR", yi))
          if bit != "":
            if findInMap(reverseLookup, getReverseLookupKey(gate.a, "XOR", carryIn)) != "":
              swapWires(pairs, gates, bit, gate.a)
              swapped = true
              break
            if findInMap(reverseLookup, getReverseLookupKey(gate.b, "XOR", carryIn)) != "":
              swapWires(pairs, gates, bit, gate.b)
              swapped = true
              break
      elif foundAdder != zi:
        swapWires(pairs, gates, foundAdder, zi)
        swapped = true
        break
      if swapped: break
      carryIn = carryOut
    if swapped: continue
  var parts: seq[string]
  for p in pairs:
    parts.add p.a
    parts.add p.b
  parts.sort(system.cmp)
  result = parts.join(",")

when isMainModule:
  let input = readFile("input.txt")
  var gates = parse(input)
  if gates.len == 0: quit(1)
  echo solution(gates)
