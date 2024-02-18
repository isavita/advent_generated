
fs = require 'fs'

execute = (memory) ->
  for i in [0...memory.length] by 4
    switch memory[i]
      when 1 then memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]]
      when 2 then memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]]
      when 99  then return memory[0]
  return memory[0]

data = fs.readFileSync('input.txt').toString().trim().split(',').map (x) -> parseInt(x)

original = data.slice()

for noun in [0...100]
  for verb in [0...100]
    memory = original.slice()
    memory[1] = noun
    memory[2] = verb
    if execute(memory) == 19690720 then console.log 100 * noun + verb; return
