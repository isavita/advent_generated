
import std/[strutils, sequtils, streams, math, sugar]

type
  Mode = enum
    Position, Immediate, Relative
  
  VM = ref object
    code: seq[int]
    ip: int
    relativeBase: int

proc newVM(filename: string): VM =
  result = VM(code: @[], ip: 0, relativeBase: 0)
  let file = openFileStream(filename)
  result.code = file.readAll().strip().split(",").map(parseInt)
  result.code.setLen(result.code.len * 10)  # Pre-allocate extra memory

proc getParamAddress(vm: var VM, pos: int, mode: Mode): int =
  case mode
  of Position: result = vm.code[pos]
  of Immediate: result = pos
  of Relative: result = vm.relativeBase + vm.code[pos]

proc run(vm: var VM): seq[int] =
  var output: seq[int]
  let instructions = [
    "NOT A J",
    "NOT B T",
    "OR T J", 
    "NOT C T",
    "OR T J",
    "AND D J",
    "WALK"
  ]

  var inputIndex = 0
  var inputStr = instructions.join("\n") & "\n"

  while true:
    let opCode = vm.code[vm.ip] mod 100
    let cmd = vm.code[vm.ip]

    case opCode
    of 1:  # add
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      let p2 = vm.getParamAddress(vm.ip + 2, Mode((cmd div 1000) mod 10))
      let p3 = vm.getParamAddress(vm.ip + 3, Mode((cmd div 10000) mod 10))
      vm.code[p3] = vm.code[p1] + vm.code[p2]
      vm.ip += 4

    of 2:  # multiply
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      let p2 = vm.getParamAddress(vm.ip + 2, Mode((cmd div 1000) mod 10))
      let p3 = vm.getParamAddress(vm.ip + 3, Mode((cmd div 10000) mod 10))
      vm.code[p3] = vm.code[p1] * vm.code[p2]
      vm.ip += 4

    of 3:  # input
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      if inputIndex < inputStr.len:
        vm.code[p1] = inputStr[inputIndex].ord
        inc(inputIndex)
      vm.ip += 2

    of 4:  # output
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      output.add(vm.code[p1])
      if vm.code[p1] > 127:
        return output
      vm.ip += 2

    of 5:  # jump-if-true
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      let p2 = vm.getParamAddress(vm.ip + 2, Mode((cmd div 1000) mod 10))
      if vm.code[p1] != 0:
        vm.ip = vm.code[p2]
      else:
        vm.ip += 3

    of 6:  # jump-if-false
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      let p2 = vm.getParamAddress(vm.ip + 2, Mode((cmd div 1000) mod 10))
      if vm.code[p1] == 0:
        vm.ip = vm.code[p2]
      else:
        vm.ip += 3

    of 7:  # less than
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      let p2 = vm.getParamAddress(vm.ip + 2, Mode((cmd div 1000) mod 10))
      let p3 = vm.getParamAddress(vm.ip + 3, Mode((cmd div 10000) mod 10))
      vm.code[p3] = if vm.code[p1] < vm.code[p2]: 1 else: 0
      vm.ip += 4

    of 8:  # equals
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      let p2 = vm.getParamAddress(vm.ip + 2, Mode((cmd div 1000) mod 10))
      let p3 = vm.getParamAddress(vm.ip + 3, Mode((cmd div 10000) mod 10))
      vm.code[p3] = if vm.code[p1] == vm.code[p2]: 1 else: 0
      vm.ip += 4

    of 9:  # adjust relative base
      let p1 = vm.getParamAddress(vm.ip + 1, Mode((cmd div 100) mod 10))
      vm.relativeBase += vm.code[p1]
      vm.ip += 2

    of 99:  # halt
      break

    else:
      raise newException(ValueError, "Invalid opcode")

proc main() =
  var vm = newVM("input.txt")
  let output = vm.run()
  echo output[^1]

main()
