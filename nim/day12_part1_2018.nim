
import strutils, sequtils, tables

proc minMaxKeys(m: Table[int, char]): tuple[minKey, maxKey: int] =
  var first = true
  for k in m.keys:
    if first:
      result.minKey = k
      result.maxKey = k
      first = false
    else:
      if k < result.minKey:
        result.minKey = k
      if k > result.maxKey:
        result.maxKey = k

proc main() =
  var initialState = ""
  var rules: Table[string, char] = initTable[string, char]()
  var file = open("input.txt")
  for line in file.lines:
    if "initial state" in line:
      initialState = line.split(": ")[1]
    elif "=>" in line:
      let parts = line.split(" => ")
      rules[parts[0]] = parts[1][0]
  close(file)

  var state: Table[int, char] = initTable[int, char]()
  for i, c in initialState:
    if c == '#':
      state[i] = '#'

  for generation in 0..<20:
    var newState: Table[int, char] = initTable[int, char]()
    let (minPot, maxPot) = minMaxKeys(state)
    for i in minPot-2..maxPot+2:
      var pattern = ""
      for j in i-2..i+2:
        pattern.add(if state.getOrDefault(j, '.') == '#': '#' else: '.')
      if rules.getOrDefault(pattern, '.') == '#':
        newState[i] = '#'
    state = newState

  var sum = 0
  for k in state.keys:
    sum += k

  echo sum

main()
