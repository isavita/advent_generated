
import std/[strutils, tables, deques, sequtils]

type
  PulseValue = enum
    Low = 0
    High = 1
  ModulePrefix = enum
    None
    FlipFlop
    Conjunction
  Pulse = object
    value: PulseValue
    fromName: string
    toName: string
  Module = object
    name: string
    prefix: ModulePrefix
    destinations: seq[string]
    state: bool
    memory: Table[string, PulseValue]

proc parseInput(inputLines: seq[string]): Table[string, Module] =
  result = initTable[string, Module]()
  for line in inputLines:
    let arrowPos = line.find(" -> ")
    let modulePart = line[0..<arrowPos]
    let destinationsPart = line[arrowPos+4..^1]
    var module: Module
    module.state = false
    if modulePart[0] == '%':
      module.prefix = FlipFlop
      module.name = modulePart[1..^1]
    elif modulePart[0] == '&':
      module.prefix = Conjunction
      module.name = modulePart[1..^1]
    else:
      module.prefix = None
      module.name = modulePart
    module.destinations = destinationsPart.split(", ")
    result[module.name] = module
  for name, module in result:
    for destName in module.destinations:
      if result.hasKey(destName) and result[destName].prefix == Conjunction:
        result[destName].memory[name] = Low

proc pushButton(modules: var Table[string, Module], numCycles: int): (int64, int64) =
  var cntLow: int64 = 0
  var cntHigh: int64 = 0
  var pulseQueue = initDeque[Pulse]()
  for cycle in 0..<numCycles:
    pulseQueue.addLast(Pulse(value: Low, fromName: "button", toName: "broadcaster"))
    while pulseQueue.len > 0:
      let currentPulse = pulseQueue.popFirst()
      if currentPulse.value == Low:
        cntLow += 1
      else:
        cntHigh += 1
      if not modules.hasKey(currentPulse.toName):
        continue
      var module = modules[currentPulse.toName]
      var newPulseValue: PulseValue
      if module.prefix == FlipFlop:
        if currentPulse.value == Low:
          module.state = not module.state
          newPulseValue = if module.state: High else: Low
        else:
          continue
      elif module.prefix == Conjunction:
        module.memory[currentPulse.fromName] = currentPulse.value
        var allHigh = true
        for memVal in module.memory.values:
          if memVal == Low:
            allHigh = false
            break
        newPulseValue = if allHigh: Low else: High
      else:
        newPulseValue = currentPulse.value
      for destName in module.destinations:
        pulseQueue.addLast(Pulse(value: newPulseValue, fromName: module.name, toName: destName))
      modules[module.name] = module
  return (cntLow, cntHigh)

proc solve(inputLines: seq[string]): int64 =
  var modules = parseInput(inputLines)
  let numCycles = 1000
  let counts = pushButton(modules, numCycles)
  return counts[0] * counts[1]

proc readFile(fileName: string): seq[string] =
  result = @[]
  let file = open(fileName)
  for line in file.lines:
    result.add(line)
  file.close()

echo solve(readFile("input.txt"))
