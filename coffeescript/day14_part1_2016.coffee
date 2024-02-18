
# Load the required libraries
fs = require 'fs'
crypto = require 'crypto'

# Read the input file
input = fs.readFileSync('input.txt', 'utf8').trim()

# Initialize variables
keysFound = 0
index = 0
triple = null

# Generate hashes until we find 64 keys
while keysFound < 64
  # Generate the hash for the current index
  hash = crypto.createHash('md5').update(input + index).digest('hex')

  # Check if the hash contains a triple
  for i in [0...hash.length - 2]
    if hash[i] == hash[i + 1] == hash[i + 2]
      triple = hash[i]
      break

  # If we found a triple, check if the next 1000 hashes contain five of the same character
  if triple
    for j in [index + 1...index + 1001]
      hash = crypto.createHash('md5').update(input + j).digest('hex')
      if hash.match(new RegExp(triple + '{5}'))
        keysFound++
        console.log index if keysFound == 64
        triple = null
        break

  # Increment the index
  index++
