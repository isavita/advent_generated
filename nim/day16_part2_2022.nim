
import strutils, strscans, sequtils, tables

proc main() =
  let data = readFile("input.txt").strip().splitLines()
  var names: seq[string]
  var flows: seq[int]
  var adjRaw: seq[string]
  var nameToIdx = newTable[string, int]()

  for line in data:
    var name, rest: string
    var f: int
    if line.scanf("Valve $w has flow rate=$i; $+", name, f, rest):
      nameToIdx[name] = names.len
      names.add(name)
      flows.add(f)
      adjRaw.add(rest)

  let n = names.len
  var dist = newSeqWith(n, newSeqWith(n, 100))
  for i in 0..<n:
    dist[i][i] = 0
    let s = adjRaw[i]
    let p = if "valves" in s: s.split("valves")[1] else: s.split("valve")[1]
    for v in p.split(","):
      let vn = v.strip()
      if vn != "": dist[i][nameToIdx[vn]] = 1

  for k in 0..<n:
    for i in 0..<n:
      for j in 0..<n:
        dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

  var fv: seq[int]
  for i in 0..<n:
    if flows[i] > 0: fv.add(i)

  let m = fv.len
  var mp = newSeq[int](1 shl m)

  proc dfs(u, t, mask, p: int) =
    if p > mp[mask]: mp[mask] = p
    for i in 0..<m:
      let v = fv[i]
      let nt = t - dist[u][v] - 1
      if (mask and (1 shl i)) == 0 and nt > 0:
        dfs(v, nt, mask or (1 shl i), p + nt * flows[v])

  if nameToIdx.contains("AA"):
    dfs(nameToIdx["AA"], 26, 0, 0)

  for i in 0..<m:
    for mask in 0..<(1 shl m):
      if (mask and (1 shl i)) != 0:
        mp[mask] = max(mp[mask], mp[mask xor (1 shl i)])

  var ans = 0
  let full = (1 shl m) - 1
  for i in 0..<(1 shl m):
    ans = max(ans, mp[i] + mp[full xor i])
  echo ans

main()
