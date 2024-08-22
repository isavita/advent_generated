import os
import strutils

type
  Monkey = ref object
    name: string
    number: int
    left: Monkey
    right: Monkey
    operation: char

proc parseMonkey(line: string): Monkey =
  let parts = line.split(": ")
  let name = parts[0]
  let job = parts[1]

  if job.contains(" + ") or job.contains(" - ") or job.contains(" * ") or job.contains(" / "):
    let opIndex = job.find(' ')
    let left = job[0..<opIndex]
    let operation = job[opIndex + 1]
    let right = job[opIndex + 3..<job.len]
    return Monkey(name: name, operation: operation, left: Monkey(name: left), right: Monkey(name: right))
  else:
    let number = parseInt(job)
    return Monkey(name: name, number: number)

proc buildMonkeyTree(monkeys: seq[Monkey], name: string): Monkey =
  for monkey in monkeys:
    if monkey.name == name:
      if monkey.left != nil and monkey.right != nil:
        monkey.left = buildMonkeyTree(monkeys, monkey.left.name)
        monkey.right = buildMonkeyTree(monkeys, monkey.right.name)
      return monkey
  return nil

proc evaluate(monkey: Monkey): int =
  if monkey.operation == '\0':
    return monkey.number
  let left = evaluate(monkey.left)
  let right = evaluate(monkey.right)
  case monkey.operation
  of '+': return left + right
  of '-': return left - right
  of '*': return left * right
  of '/': return left div right
  else: return 0

let file = "input.txt"
let lines = readFile(file).splitLines()
var monkeys: seq[Monkey] = @[]
for line in lines:
  monkeys.add(parseMonkey(line))

let root = buildMonkeyTree(monkeys, "root")
echo evaluate(root)