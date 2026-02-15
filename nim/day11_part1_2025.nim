
import strutils
import tables
import os

proc count(node: string, adj: Table[string, seq[string]], memo: var Table[string, int64]): int64 =
  if node == "out":
    return 1
  if memo.hasKey(node):
    return memo[node]
  var total = 0'i64
  if adj.hasKey(node):
    for nxt in adj[node]:
      total += count(nxt, adj, memo)
  memo[node] = total
  return total

proc main() =
  var adj = initTable[string, seq[string]]()
  var memo = initTable[string, int64]()
  try:
    let content = readFile("input.txt")
    for line in content.splitLines():
      if line.strip().len == 0:
        continue
      let parts = line.split(':')
      if parts.len != 2:
        quit("Invalid input format")
      let src = parts[0].strip()
      let targets = parts[1].strip().splitWhitespace()
      adj[src] = targets
    echo count("you", adj, memo)
  except IOError:
    quit("File 'input.txt' not found.")

when isMainModule:
  main()
