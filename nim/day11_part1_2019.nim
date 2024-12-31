
import strutils, sequtils, tables

type
  PanelColor = enum Black, White
  Direction = enum Up, Right, Down, Left
  Position = tuple[x, y: int]
  Robot = object
    pos: Position
    dir: Direction
  Intcode = object
    memory: seq[int]
    ip: int
    input: seq[int]
    output: seq[int]
    halted: bool
  Grid = Table[Position, PanelColor]

proc newIntcode(program: seq[int]): Intcode =
  Intcode(memory: program, ip: 0, input: @[], output: @[], halted: false)

proc addInput(ic: var Intcode, input: int) =
  ic.input.add(input)

proc turnAndMove(r: var Robot, turnDirection: int) =
  if turnDirection == 0:
    r.dir = Direction((ord(r.dir) + 3) mod 4)
  else:
    r.dir = Direction((ord(r.dir) + 1) mod 4)

  case r.dir:
    of Up: r.pos.y -= 1
    of Right: r.pos.x += 1
    of Down: r.pos.y += 1
    of Left: r.pos.x -= 1

proc ensureMemory(ic: var Intcode, address: int) =
  if address >= ic.memory.len:
    ic.memory.setLen(address + 1)

proc readMemory(ic: var Intcode, address: int): int =
  ic.ensureMemory(address)
  ic.memory[address]

proc writeMemory(ic: var Intcode, address: int, value: int) =
  ic.ensureMemory(address)
  ic.memory[address] = value

proc getParams(ic: Intcode, count: int): seq[int] =
  var paramModes = ic.memory[ic.ip] div 100
  var params: seq[int] = @[]
  for i in 0..<count:
    if paramModes mod 10 == 1:
      params.add(ic.ip + i + 1)
    else:
      params.add(ic.memory[ic.ip + i + 1])
    paramModes = paramModes div 10
  return params

proc run(ic: var Intcode) =
  ic.output = @[]
  while true:
    let opcode = ic.memory[ic.ip] mod 100
    case opcode:
      of 1, 2, 7, 8:
        ic.ensureMemory(ic.ip + 3)
        let params = ic.getParams(3)
        let val1 = ic.readMemory(params[0])
        let val2 = ic.readMemory(params[1])
        if opcode == 1:
          ic.writeMemory(params[2], val1 + val2)
        elif opcode == 2:
          ic.writeMemory(params[2], val1 * val2)
        elif opcode == 7 and val1 < val2 or opcode == 8 and val1 == val2:
          ic.writeMemory(params[2], 1)
        else:
          ic.writeMemory(params[2], 0)
        ic.ip += 4
      of 3, 4:
        ic.ensureMemory(ic.ip + 1)
        let params = ic.getParams(1)
        if opcode == 3:
          if ic.input.len == 0:
            return
          ic.writeMemory(params[0], ic.input[0])
          ic.input = ic.input[1..ic.input.high]
        else:
          ic.output.add(ic.readMemory(params[0]))
        ic.ip += 2
      of 5, 6:
        ic.ensureMemory(ic.ip + 2)
        let params = ic.getParams(2)
        let val = ic.readMemory(params[0])
        let target = ic.readMemory(params[1])
        if (opcode == 5 and val != 0) or (opcode == 6 and val == 0):
          ic.ip = target
        else:
          ic.ip += 3
      of 99:
        ic.halted = true
        return
      else:
        raise newException(ValueError, "unknown opcode: " & $opcode)

proc outputs(ic: Intcode): seq[int] =
  ic.output

proc halted(ic: Intcode): bool =
  ic.halted

let data = readFile("input.txt").strip().split(",")
var program = data.map(parseInt)

var grid: Grid = initTable[Position, PanelColor]()
var robot = Robot(pos: (x: 0, y: 0), dir: Up)
var intcode = newIntcode(program)

while not intcode.halted():
  let currentColor = grid.getOrDefault(robot.pos, Black)
  intcode.addInput(ord(currentColor))
  intcode.run()
  let outputs = intcode.outputs()

  if outputs.len == 2:
    grid[robot.pos] = PanelColor(outputs[0])
    robot.turnAndMove(outputs[1])

echo grid.len
