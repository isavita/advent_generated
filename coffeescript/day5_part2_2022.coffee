fs = require 'fs'

readAll = (path) ->
  fs.readFileSync(path, 'utf8').trim()

move = (stacks, steps) ->
  stacks.forEach (stack, i) ->
    stack.reverse()
  steps.forEach (step) ->
    [n, from, to] = step.match(/\d+/g).map(Number)
    from--
    to--
    stacks[to] = stacks[to].concat(stacks[from].splice(-n, n))
  stacks.map((stack) -> stack[stack.length - 1]).join('')

[input, steps] = readAll('input.txt').split('\n\n')
input = input.split('\n')
stacks = Array.from({ length: (input[0].length + 1) / 4 }, () -> [])
input.forEach (line) ->
  line.split('').forEach (ch, i) ->
    if ch >= 'A' and ch <= 'Z'
      stacks[(i - 1) / 4].push(ch)

steps = steps.split('\n')
console.log move(stacks, steps)