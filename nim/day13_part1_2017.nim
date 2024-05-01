import sequtils, strutils, algorithm

type
  Layer = tuple[depth: int, range: int]

proc parseInput(file: string): seq[Layer] =
  var layers: seq[Layer] = @[]
  for line in lines(file):
    let parts = line.split(": ")
    let depth = parseInt(parts[0])
    let range = parseInt(parts[1])
    layers.add((depth, range))
  layers

proc calculateSeverity(layers: seq[Layer]): int =
  var severity = 0
  for layer in layers:
    if layer.depth mod (2 * (layer.range - 1)) == 0:
      severity += layer.depth * layer.range
  severity

when isMainModule:
  let layers = parseInput("input.txt")
  let severity = calculateSeverity(layers)
  echo "Severity: ", severity