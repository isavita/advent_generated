import strutils

const
  workers = 5
  baseTime = 60

type Adj = array[0..25, seq[int]]
type Indeg = array[0..25, int]
type Present = array[0..25, bool]

proc addAvailable*(avail: var seq[int], t: int) =
  avail.add(t)
  var i = avail.len - 1
  while i > 0 and avail[i - 1] < avail[i]:
    let tmp = avail[i - 1]
    avail[i - 1] = avail[i]
    avail[i] = tmp
    dec(i)

proc main() =
  var g: Adj
  for i in 0..25:
    g[i] = @[]
  var indeg: Indeg
  var present: Present
  for i in 0..25:
    indeg[i] = 0
    present[i] = false

  let content = readFile("input.txt")
  let lines = content.split("\n")
  for line in lines:
    if line.len == 0: discard
    let parts = line.split(" ")
    if parts.len >= 8 and parts[0] == "Step":
      let aIdx = ord(parts[1][0]) - ord('A')
      let bIdx = ord(parts[7][0]) - ord('A')
      g[aIdx].add(bIdx)
      indeg[bIdx] += 1
      present[aIdx] = true
      present[bIdx] = true

  var available: seq[int] = @[]
  for i in 0..25:
    if present[i] and indeg[i] == 0:
      addAvailable(available, i)

  var inTasks: seq[int] = @[]
  var inRem: seq[int] = @[]
  var timeElapsed = 0

  while available.len > 0 or inRem.len > 0:
    while available.len > 0 and inRem.len < workers:
      let t = available[available.len - 1]
      setLen(available, available.len - 1)
      inTasks.add(t)
      inRem.add(baseTime + t + 1)

    if inRem.len == 0:
      break

    var minLeft = inRem[0]
    for i in 1 .. inRem.len - 1:
      if inRem[i] < minLeft:
        minLeft = inRem[i]

    timeElapsed += minLeft

    var newTasks: seq[int] = @[]
    var newRem: seq[int] = @[]
    for i in 0 .. inRem.len - 1:
      let r = inRem[i] - minLeft
      if r > 0:
        newTasks.add(inTasks[i])
        newRem.add(r)
      else:
        let t = inTasks[i]
        for next in g[t]:
          indeg[next] -= 1
          if indeg[next] == 0 and present[next]:
            addAvailable(available, next)
    inTasks = newTasks
    inRem = newRem

  echo timeElapsed

when isMainModule:
  main()