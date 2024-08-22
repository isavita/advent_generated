import strutils, sequtils

type Component = tuple[port1: int, port2: int]

proc parseComponents(filename: string): seq[Component] =
  var components: seq[Component] = @[]
  for line in lines(filename):
    let ports = line.split("/")
    let port1 = parseInt(ports[0])
    let port2 = parseInt(ports[1])
    components.add((port1, port2))
  components

proc findStrongestBridge(components: seq[Component], port: int, used: seq[Component]): int =
  var maxStrength = 0
  for component in components:
    if component.port1 == port or component.port2 == port:
      var newUsed = used
      newUsed.add(component)
      var newComponents = components.filterIt(it != component)
      let nextPort = if component.port1 == port: component.port2 else: component.port1
      let strength = component.port1 + component.port2 + findStrongestBridge(newComponents, nextPort, newUsed)
      if strength > maxStrength:
        maxStrength = strength
  maxStrength

proc findLongestBridge(components: seq[Component], port: int, used: seq[Component]): (int, int) =
  var maxLength = 0
  var maxStrength = 0
  for component in components:
    if component.port1 == port or component.port2 == port:
      var newUsed = used
      newUsed.add(component)
      var newComponents = components.filterIt(it != component)
      let nextPort = if component.port1 == port: component.port2 else: component.port1
      let (length, strength) = findLongestBridge(newComponents, nextPort, newUsed)
      if length + 1 > maxLength:
        maxLength = length + 1
        maxStrength = strength + component.port1 + component.port2
      elif length + 1 == maxLength and strength + component.port1 + component.port2 > maxStrength:
        maxStrength = strength + component.port1 + component.port2
  (maxLength, maxStrength)

let components = parseComponents("input.txt")
let strongestBridge = findStrongestBridge(components, 0, @[])
echo "Strongest bridge strength: ", strongestBridge

let (longestLength, longestStrength) = findLongestBridge(components, 0, @[])
echo "Longest bridge strength: ", longestStrength