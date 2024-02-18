
fs = require('fs')
readline = require('readline')

# Read input file and split it into lines
inputStream = fs.createReadStream('input.txt')
rl = readline.createInterface({input: inputStream})
lines = []
rl.on('line', (line) -> lines.push line).on('close', () ->

  # Count lines that support SSL
  sslCount = 0
  for line in lines
    if supportsSSL line
      sslCount++

  # Print the result
  console.log sslCount
)

# Check if a string supports SSL
supportsSSL = (ip) ->
  # Find all bracketed substrings and replace them with '-'
  insideBrackets = /\[[a-z]+\]/g
  bracketContents = ip.match(insideBrackets)
  ip = ip.replace(insideBrackets, '-')

  # Find all ABAs outside brackets
  abas = findABAs ip
  for aba in abas
    bab = aba[1] + aba[0] + aba[1]
    if bracketContents.some (bc) -> bc.includes bab
      return true
  return false

# Find all ABAs in a string
findABAs = (s) ->
  abas = []
  for i in [0...s.length - 2]
    if s[i] isnt s[i+1] and s[i] is s[i+2]
      abas.push s.slice(i, i+3)
  return abas
