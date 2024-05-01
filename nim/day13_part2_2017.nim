import sequtils, strutils, math

type
  Layer = object
    depth: int
    range: int
    scanner: int

proc parseInput(): seq[Layer] =
  let input = readFile("input.txt").splitLines()
  var layers: seq[Layer] = @[]
  for line in input:
    let parts = line.split(": ")
    let depth = parseInt(parts[0])
    let range = parseInt(parts[1])
    layers.add(Layer(depth: depth, range: range, scanner: 0))
  layers

proc moveScanners(layers: var seq[Layer]) =
  for layer in layers.mitems:
    layer.scanner = (layer.scanner + 1) mod (layer.range * 2 - 2)

proc isCaught(layers: seq[Layer], delay: int): bool =
  for layer in layers:
    if (layer.depth + delay) mod (layer.range * 2 - 2) == 0:
      return true
  false

proc part1(layers: seq[Layer]): int =
  var severity = 0
  for layer in layers:
    if layer.scanner == 0:
      severity += layer.depth * layer.range
  severity

proc part2(layers: seq[Layer]): int =
  var delay = 0
  while isCaught(layers, delay):
    delay += 1
  delay

let layers = parseInput()
echo "Part 1: ", part1(layers)
echo "Part 2: ", part2(layers)