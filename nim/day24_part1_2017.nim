import sequtils, strutils, algorithm

type Component = tuple[port1, port2: int]

proc parseComponents(filename: string): seq[Component] =
  for line in lines filename:
    let parts = line.split("/")
    result.add((parseInt(parts[0]), parseInt(parts[1])))

proc findStrongestBridge(components: seq[Component], currentPort: int, used: seq[Component]): int =
  var maxStrength = 0
  for component in components:
    if component.port1 == currentPort or component.port2 == currentPort:
      let newUsed = used & @[component]
      let newComponents = components.filterIt(it != component)
      let newPort = if component.port1 == currentPort: component.port2 else: component.port1
      let strength = component.port1 + component.port2 + findStrongestBridge(newComponents, newPort, newUsed)
      maxStrength = max(maxStrength, strength)
  return maxStrength

when isMainModule:
  let components = parseComponents("input.txt")
  echo "Strongest bridge strength: ", findStrongestBridge(components, 0, @[])