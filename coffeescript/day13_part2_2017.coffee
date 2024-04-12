fs = require 'fs'

class Scanner
  constructor: (@range) ->
    @position = 0
    @direction = 1

passThrough = (firewall, delay) ->
  for depth, scanner of firewall
    if (parseInt(depth) + delay) % (2 * (scanner.range - 1)) == 0
      return false
  true

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  firewall = {}
  lines = data.trim().split '\n'
  for line in lines
    [depth, rng] = line.split ': '
    firewall[depth] = new Scanner parseInt(rng)

  delay = 0
  delay++ until passThrough firewall, delay
  console.log delay