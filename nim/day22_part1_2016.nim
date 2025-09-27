import os, strutils

type Node = object
  x, y, size, used, avail: int

proc toInt(s: string): int =
  var i = 0
  var sign = 1
  if s.len > 0 and s[0] == '-':
    sign = -1
    i = 1
  var v = 0
  while i < s.len:
    let ch = s[i]
    if ch >= '0' and ch <= '9':
      v = v * 10 + (int(ch) - 48)
    i += 1
  return sign * v

let content = readFile("input.txt")
var nodes: seq[Node] = @[]
let prefix = "/dev/grid/node-x"

let lines = content.splitLines
for line in lines:
  let tokens = splitWhitespace(line)
  if tokens.len >= 4:
    let first = tokens[0]
    if first.startsWith(prefix):
      let pos = first.find("-y")
      if pos >= 0:
        var n = Node(x: 0, y: 0, size: 0, used: 0, avail: 0)
        n.x = toInt(first[prefix.len .. pos - 1])
        n.y = toInt(first[(pos + 2) .. first.len - 1])
        var sTok = tokens[1]
        if sTok.endsWith("T"): sTok = sTok[0 .. sTok.len - 2]
        n.size = toInt(sTok)
        var uTok = tokens[2]
        if uTok.endsWith("T"): uTok = uTok[0 .. uTok.len - 2]
        n.used = toInt(uTok)
        var aTok = tokens[3]
        if aTok.endsWith("T"): aTok = aTok[0 .. aTok.len - 2]
        n.avail = toInt(aTok)
        nodes.add(n)

var viable = 0
let nNodes = nodes.len
for i in 0 ..< nNodes:
  if nodes[i].used == 0: continue
  for j in 0 ..< nNodes:
    if i == j: continue
    if nodes[i].used <= nodes[j].avail:
      inc viable

echo "Number of viable pairs: ", viable