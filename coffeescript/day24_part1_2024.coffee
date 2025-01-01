
fs = require 'fs'

try
  input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
catch error
  console.log "Error reading input.txt:", error
  process.exit(1)

wires = {}
gates = []

wireRegex = /^(\w+):\s*([01])$/
gateRegex = /^(\w+)\s+(AND|OR|XOR)\s+(\w+)\s+->\s+(\w+)$/

parsingWires = true
for line in input
  line = line.trim()
  if line is ''
    parsingWires = false
    continue

  if parsingWires
    matches = line.match(wireRegex)
    if not matches or matches.length != 3
      console.log "Invalid wire definition:", line
      process.exit(1)
    wireName = matches[1]
    wireValue = parseInt(matches[2], 10)
    wires[wireName] = wireValue
  else
    matches = line.match(gateRegex)
    if not matches or matches.length != 5
      console.log "Invalid gate definition:", line
      process.exit(1)
    gates.push
      input1: matches[1]
      operation: matches[2]
      input2: matches[3]
      output: matches[4]

remainingGates = gates.slice()

while remainingGates.length > 0
  progress = false
  newRemainingGates = []
  for gate in remainingGates
    val1 = wires[gate.input1]
    val2 = wires[gate.input2]
    if val1? and val2?
      switch gate.operation
        when "AND"
          outputVal = if val1 == 1 and val2 == 1 then 1 else 0
        when "OR"
          outputVal = if val1 == 1 or val2 == 1 then 1 else 0
        when "XOR"
          outputVal = if val1 != val2 then 1 else 0
        else
          console.log "Unknown operation:", gate.operation
          process.exit(1)
      wires[gate.output] = outputVal
      progress = true
    else
      newRemainingGates.push(gate)
  if not progress
    console.log "Cannot evaluate remaining gates due to missing inputs or cyclic dependencies."
    process.exit(1)
  remainingGates = newRemainingGates

zWires = {}
zRegex = /^z(\d+)$/
for wire, val of wires
  matches = wire.match(zRegex)
  if matches and matches.length == 2
    index = parseInt(matches[1], 10)
    zWires[index] = val

if Object.keys(zWires).length == 0
  console.log "No wires starting with 'z' found."
  process.exit(1)

indices = Object.keys(zWires).map(Number).sort((a, b) -> b - a)

binaryString = indices.map((idx) -> zWires[idx]).join('')

decimalValue = parseInt(binaryString, 2)

console.log decimalValue
