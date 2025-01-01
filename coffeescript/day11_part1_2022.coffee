
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim()

monkeys = []
monkey_data = input.split('\n\n')

for monkey_str in monkey_data
  lines = monkey_str.split('\n')
  monkey = {}
  monkey.items = lines[1].match(/Starting items: ([\d, ]+)/)[1].split(', ').map(Number)
  operation_match = lines[2].match(/Operation: new = (old) ([+\*]) ([\dold]+)/)
  monkey.operation = {
    operand1: operation_match[1]
    operator: operation_match[2]
    operand2: operation_match[3]
  }
  monkey.test = parseInt(lines[3].match(/Test: divisible by (\d+)/)[1])
  monkey.if_true = parseInt(lines[4].match(/If true: throw to monkey (\d+)/)[1])
  monkey.if_false = parseInt(lines[5].match(/If false: throw to monkey (\d+)/)[1])
  monkey.inspected_count = 0
  monkeys.push monkey

rounds = 20

for round in [1..rounds]
  for monkey in monkeys
    while monkey.items.length > 0
      item = monkey.items.shift()
      monkey.inspected_count++
      
      old = item
      if monkey.operation.operand2 == 'old'
        operand2 = old
      else
        operand2 = parseInt(monkey.operation.operand2)
      
      if monkey.operation.operator == '+'
        new_worry = old + operand2
      else
        new_worry = old * operand2
      
      new_worry = Math.floor(new_worry / 3)
      
      if new_worry % monkey.test == 0
        monkeys[monkey.if_true].items.push new_worry
      else
        monkeys[monkey.if_false].items.push new_worry

inspected_counts = monkeys.map (monkey) -> monkey.inspected_count
inspected_counts.sort (a, b) -> b - a

monkey_business = inspected_counts[0] * inspected_counts[1]

console.log monkey_business
