
import strutils
import tables
import algorithm
import sequtils

type Valve = object
  id: string
  flow: int
  tunnels: Table[string, int]

proc readAll(filename: string): string =
  readFile(filename).strip()

proc maxPressure(valves: Table[string, Valve], curr: string, minute: int, pressure: int, openValves: seq[string]): int =
  var maxP = pressure
  for next in openValves:
    let newOpen = openValves.filterIt(it != next)
    let timeLeft = minute - valves[curr].tunnels[next] - 1
    if timeLeft > 0:
      maxP = max(maxP, maxPressure(valves, next, timeLeft, timeLeft * valves[next].flow + pressure, newOpen))
  maxP

proc divide(l: int): seq[(seq[int], seq[int])] =
  if l == 1:
    return @[(@[], @[0]), (@[0], @[])]
  let d = divide(l - 1)
  var r: seq[(seq[int], seq[int])] = @[]
  for (left, right) in d:
    r.add((@[l - 1] & left, right))
    r.add((left, @[l - 1] & right))
  r

proc main() =
  let input = readAll("input.txt")
  var valves: Table[string, Valve] = initTable[string, Valve]()
  for line in input.splitLines():
    let sp = line.split("; ")
    var v: Valve
    let match = sp[0].split(" ")
    v.id = match[1]
    v.flow = parseInt(match[4].split("=")[1])
    v.tunnels = initTable[string, int]()
    let tunnels = sp[1].replace("tunnel leads to valve ", "").replace("tunnels lead to valves ", "").split(", ")
    for tunnel in tunnels:
      v.tunnels[tunnel] = 1
    v.tunnels[v.id] = 0
    valves[v.id] = v

  for k in valves.keys():
    for i in valves.keys():
      for j in valves.keys():
        let dik = valves[i].tunnels.getOrDefault(k, int.high)
        let dkj = valves[k].tunnels.getOrDefault(j, int.high)
        if dik != int.high and dkj != int.high:
          let dij = valves[i].tunnels.getOrDefault(j, int.high)
          if dij == int.high or dij > dik + dkj:
            valves[i].tunnels[j] = dik + dkj

  let openValves = toSeq(valves.values()).filterIt(it.flow > 0).mapIt(it.id)
  echo maxPressure(valves, "AA", 30, 0, openValves)

when isMainModule:
  main()
