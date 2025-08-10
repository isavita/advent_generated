
import std/[strutils, tables, os]

const
  MaxNodes = 2000

var
  edges: seq[(string, string)] = @[]

# read and parse input
for line in readFile("input.txt").splitLines:
  let h = line.find('-')
  if h == -1: continue
  let n1 = line[0 ..< h].strip
  let n2 = line[h+1 .. ^1].strip
  if n1.len == 0 or n2.len == 0: continue
  edges.add((n1, n2))

# assign ids
var idTable = initTable[string, int]()
var names: seq[string] = @[]
for (a, b) in edges:
  for name in [a, b]:
    if not idTable.hasKey(name):
      idTable[name] = names.len
      names.add(name)

let nodeCount = names.len
var adj = newSeq[seq[bool]](nodeCount)
for i in 0 ..< nodeCount:
  adj[i] = newSeq[bool](nodeCount)

# build adjacency matrix
for (a, b) in edges:
  let i = idTable[a]
  let j = idTable[b]
  adj[i][j] = true
  adj[j][i] = true

# count triangles with at least one 't' node
var count = 0
for i in 0 ..< nodeCount:
  for j in i+1 ..< nodeCount:
    if not adj[i][j]: continue
    for k in j+1 ..< nodeCount:
      if adj[j][k] and adj[k][i]:
        if names[i][0] == 't' or names[j][0] == 't' or names[k][0] == 't':
          inc count

echo count
