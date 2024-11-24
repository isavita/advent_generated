
import std/[algorithm, sets, strutils, tables]

proc parseInput(filename: string): (Table[char, seq[char]], HashSet[char]) =
  var deps = initTable[char, seq[char]]()
  var allSteps = initHashSet[char]()

  for line in lines(filename):
    let parts = line.split()
    let a = parts[1][0]
    let b = parts[7][0]
    
    if not deps.hasKey(b):
      deps[b] = @[]
    deps[b].add(a)
    
    allSteps.incl(a)
    allSteps.incl(b)

  result = (deps, allSteps)

proc topologicalSort(deps: var Table[char, seq[char]], allSteps: HashSet[char]): string =
  var order: seq[char] = @[]
  var available: seq[char] = @[]

  # Find initial available steps
  for step in allSteps:
    if not deps.hasKey(step) or deps[step].len == 0:
      available.add(step)
  
  available.sort()

  while available.len > 0:
    # Pick the step that comes first alphabetically
    let next = available[0]
    available.delete(0)
    order.add(next)

    # Check and update the availability of the next steps
    for step in allSteps:
      if deps.hasKey(step):
        let idx = deps[step].find(next)
        if idx != -1:
          deps[step].delete(idx)
          
          if deps[step].len == 0:
            available.add(step)
    
    available.sort()

  result = order.join()

proc main() =
  var (deps, allSteps) = parseInput("input.txt")
  echo topologicalSort(deps, allSteps)

main()
