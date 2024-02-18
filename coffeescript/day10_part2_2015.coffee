
fs = require 'fs'

readInput = (filename) ->
  return new Promise (resolve, reject) ->
    data = ''
    stream = fs.createReadStream filename
    stream.on 'data', (chunk) -> data += chunk
    stream.on 'end', -> resolve data
    stream.on 'error', (err) -> reject err

lookAndSay = (sequence, iterations) ->
  for i in [0...iterations]
    sequence = nextSequence sequence
  return sequence

nextSequence = (sequence) ->
  result = ''
  i = 0
  while i < sequence.length
    count = 1
    digit = sequence[i]
    j = i + 1
    while j < sequence.length and sequence[j] is digit
      count++
      j++
    result += count.toString() + digit
    i += count
  return result

main = ->
  readInput('input.txt')
  .then (initialSequence) ->
    result = lookAndSay initialSequence, 50
    console.log result.length
  .catch (err) ->
    console.error "Error reading input: #{err}"

main()
