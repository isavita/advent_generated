import os, strutils

type Coord = tuple[r: int, c: int]

proc indexOfChar(ch: char, arr: seq[char]): int =
  for i in 0..<arr.len:
    if arr[i] == ch:
      return i
  return -1

proc nextPerm(a: var seq[int]): bool =
  if a.len <= 1:
    return false
  var i = a.len - 2
  while i >= 0 and a[i] >= a[i + 1]:
    dec i
  if i < 0:
    return false
  var j = a.len - 1
  while a[i] >= a[j]:
    dec j
  var tmp = a[i]
  a[i] = a[j]
  a[j] = tmp
  var l = i + 1
  var r = a.len - 1
  while l < r:
    tmp = a[l]
    a[l] = a[r]
    a[r] = tmp
    inc l
    dec r
  true

let dr = @[-1, 1, 0, 0]
let dc = @[0, 0, -1, 1]

proc main() =
  let content = readFile("input.txt")
  var layout: seq[string] = content.splitLines

  let R = layout.len
  var C = 0
  if R > 0:
    C = layout[0].len

  var digits: seq[char] = @[]
  var positions: seq[Coord] = @[]

  for r in 0..<R:
    let row = layout[r]
    for c in 0..<C:
      let ch = layout[r][c]
      if ch >= '0' and ch <= '9':
        digits.add(ch)
        positions.add((r: r, c: c))

  let N = digits.len
  if N == 0:
    echo 0
    return

  let idx0 = indexOfChar('0', digits)
  if idx0 == -1:
    echo 0
    return

  var distMat: seq[seq[int]] = @[]
  for i in 0..<N:
    var row: seq[int] = @[]
    for j in 0..<N:
      row.add(-1)
    distMat.add(row)

  for i in 0..<N:
    var grid: seq[seq[int]] = @[]
    for _ in 0..<R:
      var gr: seq[int] = @[]
      for _ in 0..<C:
        gr.add(-1)
      grid.add(gr)

    let sr = positions[i].r
    let sc = positions[i].c
    grid[sr][sc] = 0

    var qr: seq[int] = @[]
    var qc: seq[int] = @[]
    qr.add(sr)
    qc.add(sc)
    var head = 0

    while head < qr.len:
      let r = qr[head]
      let c = qc[head]
      inc head
      for k in 0..3:
        let nr = r + dr[k]
        let nc = c + dc[k]
        if nr >= 0 and nr < R and nc >= 0 and nc < C and layout[nr][nc] != '#' and grid[nr][nc] == -1:
          grid[nr][nc] = grid[r][c] + 1
          qr.add(nr)
          qc.add(nc)

    for j in 0..<N:
      distMat[i][j] = grid[positions[j].r][positions[j].c]

  var other: seq[int] = @[]
  for i in 0..<N:
    if i != idx0:
      other.add(i)

  if other.len == 0:
    echo 0
    return

  var minPath: int = 1_000_000_000
  while true:
    var pathLen: int = 0
    var ok = true
    var cur = idx0
    let first = other[0]
    let d0 = distMat[cur][first]
    if d0 == -1:
      ok = false
    else:
      pathLen = d0
      cur = first
    if ok:
      for t in 1..<other.len:
        let nxt = other[t]
        let d = distMat[cur][nxt]
        if d == -1:
          ok = false
          break
        pathLen += d
        cur = nxt
    if ok:
      let back = distMat[cur][idx0]
      if back == -1:
        ok = false
      else:
        pathLen += back
    if ok:
      if pathLen < minPath:
        minPath = pathLen
    if not nextPerm(other):
      break

  echo minPath

when isMainModule:
  main()