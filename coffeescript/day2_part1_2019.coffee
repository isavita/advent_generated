
fs = require 'fs'
readline = require 'readline'

inputData = []

rl = readline.createInterface({
  input: fs.createReadStream('input.txt'),
  crlfDelay: Infinity
})

rl.on('line', (line) ->
  vals = line.split(',')
  for val in vals
    inputData.push parseInt(val)
)

rl.on('close', () ->
  inputData[1] = 12
  inputData[2] = 2

  result = executeProgram(inputData)
  console.log result
)

executeProgram = (data) ->
  for i in [0...data.length - 3] by 4
    pos1 = data[i + 1]
    pos2 = data[i + 2]
    pos3 = data[i + 3]
    switch data[i]
      when 1
        sum = data[pos1] + data[pos2]
        data[pos3] = sum
      when 2
        product = data[pos1] * data[pos2]
        data[pos3] = product
      when 99
        return data[0]
      else
        throw "Invalid opcode"

  return data[0]
