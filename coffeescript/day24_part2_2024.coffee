
fs = require 'fs'

parse = (input) ->
  parts = input.split '\n\n'
  return null if parts.length != 2

  gates = []
  for line in parts[1].split '\n'
    continue if line == ''
    parts = line.split ' -> '
    continue if parts.length != 2
    gateParts = parts[0].split ' '
    continue if gateParts.length != 3
    gates.push
      gate: { a: gateParts[0], op: gateParts[1], b: gateParts[2] }
      output: parts[1]
  gates

createLookups = (gates) ->
  lookup = {}
  reverseLookup = {}

  for g in gates
    lookup[g.output] = g.gate
    inputs = [g.gate.a, g.gate.b].sort()
    key = "#{inputs[0]}_#{g.gate.op}_#{inputs[1]}"
    reverseLookup[key] = g.output
  [lookup, reverseLookup]

swap = (pairs, gates, a, b) ->
  pairs.push [a, b]
  for gate in gates
    if gate.output == a
      gate.output = b
    else if gate.output == b
      gate.output = a

getReverseLookupKey = (a, op, b) ->
  inputs = [a, b].sort()
  "#{inputs[0]}_#{op}_#{inputs[1]}"

solution = (gates) ->
  pairs = []
  numZ = 0
  for g in gates
    numZ++ if g.output.startsWith 'z'

  while pairs.length < 4
    adder = ''
    carry = ''
    [lookup, reverseLookup] = createLookups gates

    for i in [0...numZ]
      xi = "x#{i.toString().padStart(2, '0')}"
      yi = "y#{i.toString().padStart(2, '0')}"
      zi = "z#{i.toString().padStart(2, '0')}"

      if i == 0
        adder = reverseLookup[getReverseLookupKey(xi, 'XOR', yi)]
        carry = reverseLookup[getReverseLookupKey(xi, 'AND', yi)]
      else
        bit = reverseLookup[getReverseLookupKey(xi, 'XOR', yi)]
        if bit?
          adder = reverseLookup[getReverseLookupKey(bit, 'XOR', carry)]
          if adder?
            c1 = reverseLookup[getReverseLookupKey(xi, 'AND', yi)]
            c2 = reverseLookup[getReverseLookupKey(bit, 'AND', carry)]
            carry = reverseLookup[getReverseLookupKey(c1, 'OR', c2)]

      if not adder?
        gate = lookup[zi]
        bitKey = getReverseLookupKey(xi, 'XOR', yi)
        bit = reverseLookup[bitKey]
        if reverseLookup[getReverseLookupKey(gate.a, 'XOR', carry)]?
          swap pairs, gates, bit, gate.a
          break
        else if reverseLookup[getReverseLookupKey(gate.b, 'XOR', carry)]?
          swap pairs, gates, bit, gate.b
          break
      else if adder != zi
        swap pairs, gates, adder, zi
        break

  result = []
  for pair in pairs
    result.push pair[0], pair[1]
  result.sort().join ','

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error 'Error reading input file:', err
    return

  gates = parse data
  if not gates
    console.error 'Error parsing input'
    return

  console.log solution gates
