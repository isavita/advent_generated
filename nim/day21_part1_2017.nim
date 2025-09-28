import tables, strutils

type RuleMap = Table[string, string]

proc rotate(inputPattern: string): string =
  let parts = inputPattern.split("/")
  let size = parts.len
  var newparts: seq[string] = @[]
  for x in 0 ..< size:
    var s = ""
    for i in 0 ..< size:
      let idx = size - 1 - i
      s = s & $parts[idx][x]
    newparts.add(s)
  return newparts.join("/")

proc flip(inputPattern: string): string =
  let parts = inputPattern.split("/")
  var rev: seq[string] = @[]
  for i in countdown(parts.len - 1, 0):
    rev.add(parts[i])
  return rev.join("/")

proc enhance(inputPattern: string, rules: RuleMap): string =
  var pat = inputPattern
  for _ in 0 ..< 4:
    if rules.hasKey(pat):
      return rules[pat]
    pat = rotate(pat)
  pat = flip(pat)
  for _ in 0 ..< 4:
    if rules.hasKey(pat):
      return rules[pat]
    pat = rotate(pat)
  return ""

proc main() =
  var rules: RuleMap = initTable[string, string]()
  let content = readFile("input.txt")
  for line in content.split("\n"):
    let l = line.strip()
    if l.len == 0: continue
    let parts = l.split(" => ")
    if parts.len == 2:
      rules[parts[0].strip()] = parts[1].strip()

  var grid: seq[string] = @[".#.", "..#", "###"]

  for _ in 0 ..< 5:
    let n = grid.len
    let subsize = if n mod 2 == 0: 2 else: 3
    let newsize = if subsize == 2: (n div 2) * 3 else: (n div 3) * 4

    var newgrid: seq[string] = @[]
    for _ in 0 ..< newsize:
      newgrid.add("")

    var y = 0
    while y < n:
      var x = 0
      while x < n:
        var square: seq[string] = @[]
        for dy in 0 ..< subsize:
          let row = grid[y + dy]
          let segment = row[x .. x + subsize - 1]
          square.add(segment)
        let inputPattern = square.join("/")
        let newsquare = enhance(inputPattern, rules)
        let lines = newsquare.split("/")
        for dy in 0 ..< lines.len:
          let destRow = (y div subsize) * (subsize + 1) + dy
          newgrid[destRow] = newgrid[destRow] & lines[dy]
        x += subsize
      y += subsize

    grid = newgrid

  var count = 0
  for row in grid:
    for ch in row:
      if ch == '#': inc(count)
  echo count

when isMainModule:
  main()