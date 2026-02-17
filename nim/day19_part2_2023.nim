
import strutils, tables

type
  Rule = object
    v: char
    op: char
    val: int
    dest: string
  Ranges = array[4, array[2, int]]

var workflows = initTable[string, seq[Rule]]()

proc solve(wf: string, rgs: Ranges): int64 =
  if wf == "R": return 0
  if wf == "A":
    result = 1
    for i in 0..3:
      if rgs[i][0] > rgs[i][1]: return 0
      result *= (rgs[i][1] - rgs[i][0] + 1).int64
    return

  var current = rgs
  for r in workflows[wf]:
    if r.v == '\0':
      result += solve(r.dest, current)
      return

    let i = case r.v
      of 'x': 0
      of 'm': 1
      of 'a': 2
      of 's': 3
      else: 0
    
    var tr = current
    var fa = current
    if r.op == '<':
      tr[i][1] = min(current[i][1], r.val - 1)
      fa[i][0] = max(current[i][0], r.val)
    else:
      tr[i][0] = max(current[i][0], r.val + 1)
      fa[i][1] = min(current[i][1], r.val)
    
    if tr[i][0] <= tr[i][1]:
      result += solve(r.dest, tr)
    
    if fa[i][0] <= fa[i][1]:
      current = fa
    else:
      return

let content = readFile("input.txt").replace("\r\n", "\n")
let sections = content.split("\n\n")
if sections.len > 0:
  for line in sections[0].splitLines():
    if line.isEmptyOrWhitespace: continue
    let bIdx = line.find('{')
    let name = line[0..<bIdx]
    let rStr = line[bIdx+1..^2]
    var rs = newSeq[Rule]()
    for s in rStr.split(','):
      let cIdx = s.find(':')
      if cIdx != -1:
        rs.add(Rule(v: s[0], op: s[1], val: parseInt(s[2..<cIdx]), dest: s[cIdx+1..^1]))
      else:
        rs.add(Rule(v: '\0', dest: s))
    workflows[name] = rs

echo solve("in", [[1, 4000], [1, 4000], [1, 4000], [1, 4000]])

