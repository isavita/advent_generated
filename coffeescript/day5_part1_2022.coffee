fs = require 'fs'

readAll = (path) ->
  fs.readFileSync(path).toString().trim()

main = ->
  [stackInput, stepsInput] = readAll('input.txt').split('\n\n')
  input = stackInput.split('\n')
  stacks = ([] for [0...((input[0].length + 1) / 4)])

  for line in input
    for b, i in line
      if 'A' <= b <= 'Z'
        stacks[(i - 1) / 4].push b

  steps = stepsInput.split('\n')
  console.log move(stacks, steps)

move = (st, steps) ->
  stacks = ([] for i in st)

  for stack, i in st
    stacks[i] = stack.slice().reverse()

  for step in steps
    [_, n, from, to] = step.match(/move (\d+) from (\d+) to (\d+)/).map(Number)
    from--
    to--
    for i in [0...n]
      stacks[to].push stacks[from].pop()

  (stack[stack.length - 1] for stack in stacks).join('')

main()