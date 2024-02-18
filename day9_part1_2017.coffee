
fs = require 'fs'

# Step 1: Read Input
inputStream = fs.createReadStream('input.txt')

# Step 2: Initialize Variables
score = 0
depth = 0
inGarbage = false
cancelNext = false

# Step 3: Process Stream
inputStream.on('data', (chunk) ->
  for ch in chunk.toString()
    if cancelNext
      cancelNext = false
      continue

    if inGarbage
      if ch == '!'
        cancelNext = true
      else if ch == '>'
        inGarbage = false
    else
      switch ch
        when '{' then depth++
        when '}' then score += depth; depth--
        when '<' then inGarbage = true
)

# Step 4: Print Score
inputStream.on('end', ->
  console.log score
)

inputStream.on('error', (err) ->
  console.error "File reading error: #{err}"
)
