fs = require 'fs'

class Monkey
  constructor: (@items, @operation, @div, @next) ->
    @inspections = 0

  parse: (s) ->
    lines = s.split '\n'
    @items = (parseInt(item) for item in lines[1].split(': ')[1].split(', '))
    f = lines[2].split('= ')[1].split ' '
    switch f[1]
      when '+'
        switch f[2]
          when 'old'
            @operation = (old) -> old + old
          else
            value = parseInt(f[2])
            @operation = (old) -> old + value
      when '*'
        switch f[2]
          when 'old'
            @operation = (old) -> old * old
          else
            value = parseInt(f[2])
            @operation = (old) -> old * value
    @div = parseInt(lines[3].match(/\d+/)[0])
    @next = [parseInt(lines[4].match(/\d+/)[0]), parseInt(lines[5].match(/\d+/)[0])]

monkeyBusiness = (monkeys, rounds, worry) ->
  div = 1
  div *= m.div for m in monkeys
  for i in [0...rounds]
    for m in monkeys
      while m.items.length > 0
        m.inspections++
        item = m.operation(m.items.shift())
        item %= div if worry
        if item % m.div == 0
          monkeys[m.next[0]].items.push item
        else
          monkeys[m.next[1]].items.push item
  inspections = (m.inspections for m in monkeys)
  inspections.sort (a, b) -> b - a
  inspections[0] * inspections[1]

readAll = (path) ->
  fs.readFileSync(path, 'utf8')

parseIntegers = (s) ->
  parseInt s

s = readAll 'input.txt'
monkeys = []
for m in s.split '\n\n'
  monkey = new Monkey []
  monkey.parse m
  monkeys.push monkey

console.log monkeyBusiness(monkeys, 10000, true)