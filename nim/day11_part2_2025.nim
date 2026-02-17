
import strutils, tables, sequtils

var
  adj: seq[seq[int]]
  idMap = initTable[string, int]()

proc getId(s: string): int =
  if s notin idMap:
    idMap[s] = adj.len
    adj.add(newSeq[int]())
  return idMap[s]

proc dfs(u, target: int, memo: var seq[int64]): int64 =
  if u == target: return 1
  if memo[u] != -1: return memo[u]
  var res: int64 = 0
  for v in adj[u]:
    res += dfs(v, target, memo)
  memo[u] = res
  return res

proc countPaths(s, t: int): int64 =
  if s >= adj.len or t >= adj.len: return 0
  var memo = newSeqWith(adj.len, -1.int64)
  return dfs(s, t, memo)

proc solve() =
  let filename = "input.txt"
  for line in lines(filename):
    let parts = line.split(':')
    if parts.len < 2: continue
    let u = getId(parts[0].strip())
    for targetName in parts[1].strip().splitWhitespace():
      let v = getId(targetName)
      adj[u].add(v)

  let
    svr = getId("svr")
    dac = getId("dac")
    fft = getId("fft")
    outNode = getId("out")

  let s1 = countPaths(svr, dac) * countPaths(dac, fft) * countPaths(fft, outNode)
  let s2 = countPaths(svr, fft) * countPaths(fft, dac) * countPaths(dac, outNode)

  echo "Paths (svr->dac->fft->out): ", s1
  echo "Paths (svr->fft->dac->out): ", s2
  echo "Total paths visiting both: ", s1 + s2

solve()

