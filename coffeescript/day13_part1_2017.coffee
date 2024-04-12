fs = require 'fs'

class Scanner
  constructor: (@range) ->
    @position = 0
    @direction = 1

  moveScanner: ->
    if @position == 0
      @direction = 1
    else if @position == @range - 1
      @direction = -1
    @position += @direction

maxDepth = (firewall) ->
  Math.max.apply null, Object.keys(firewall)

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.log "Error reading file:", err
    return

  firewall = {}
  lines = data.trim().split '\n'
  for line in lines
    [depth, range] = line.split(': ').map (x) -> parseInt x, 10
    firewall[depth] = new Scanner range

  severity = 0
  for depth in [0..maxDepth(firewall)]
    scanner = firewall[depth]
    if scanner? and scanner.position == 0
      severity += depth * scanner.range

    for key, scanner of firewall
      scanner.moveScanner()

  console.log severity