
import std/[strutils, algorithm, sequtils, tables, deques]

proc main() =
  let lines = readFile("input.txt").splitLines()
  var pts: seq[(int, int)]
  var xs, ys: seq[int]
  for line in lines:
    let parts = line.split(',')
    if parts.len != 2: continue
    let x = parseInt(parts[0].strip())
    let y = parseInt(parts[1].strip())
    pts.add((x, y))
    xs.add(x)
    ys.add(y)

  if pts.len == 0:
    echo "Largest valid area: 0"
    return

  let ux = xs.deduplicate().sorted()
  let uy = ys.deduplicate().sorted()
  var xIdx, yIdx: Table[int, int]
  for i, v in ux: xIdx[v] = i
  for i, v in uy: yIdx[v] = i

  let W = 2 * ux.len + 1
  let H = 2 * uy.len + 1
  var colW = newSeq[int](W)
  var rowH = newSeq[int](H)
  colW[0] = 1
  for i in 0..<ux.len:
    colW[2 * i + 1] = 1
    let gap = if i + 1 < ux.len: ux[i + 1] - ux[i] - 1 else: 0
    colW[2 * i + 2] = max(gap, 0)
  rowH[0] = 1
  for i in 0..<uy.len:
    rowH[2 * i + 1] = 1
    let gap = if i + 1 < uy.len: uy[i + 1] - uy[i] - 1 else: 0
    rowH[2 * i + 2] = max(gap, 0)

  var grid = newSeq[seq[byte]](H)
  for i in 0..<H: grid[i] = newSeq[byte](W)

  let n = pts.len
  for i in 0..<n:
    let (ax, ay) = pts[i]
    let (bx, by) = pts[(i + 1) mod n]
    let gx1 = 2 * xIdx[ax] + 1
    let gy1 = 2 * yIdx[ay] + 1
    let gx2 = 2 * xIdx[bx] + 1
    let gy2 = 2 * yIdx[by] + 1
    if gx1 == gx2:
      let y0 = min(gy1, gy2)
      let y1 = max(gy1, gy2)
      for y in y0..y1:
        if rowH[y] != 0: grid[y][gx1] = 1
    else:
      let x0 = min(gx1, gx2)
      let x1 = max(gx1, gx2)
      for x in x0..x1:
        if colW[x] != 0: grid[gy1][x] = 1

  var dq = initDeque[(int, int)]()
  dq.addLast((0, 0))
  grid[0][0] = 2
  let dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
  while dq.len > 0:
    let (cx, cy) = dq.popFirst()
    for (dx, dy) in dirs:
      let nx = cx + dx
      let ny = cy + dy
      if nx >= 0 and nx < W and ny >= 0 and ny < H and grid[ny][nx] == 0:
        grid[ny][nx] = 2
        dq.addLast((nx, ny))

  var P = newSeq[seq[int]](H)
  for i in 0..<H: P[i] = newSeq[int](W)
  for y in 0..<H:
    var rowSum = 0
    for x in 0..<W:
      let v = if grid[y][x] != 2: colW[x] * rowH[y] else: 0
      rowSum += v
      let above = if y > 0: P[y - 1][x] else: 0
      P[y][x] = rowSum + above

  var maxArea = 0
  for i in 0..<n:
    for j in i..<n:
      let (ax, ay) = pts[i]
      let (bx, by) = pts[j]
      let w = abs(ax - bx) + 1
      let h = abs(ay - by) + 1
      let area = w * h
      if area <= maxArea: continue
      var gx1 = 2 * xIdx[ax] + 1
      var gy1 = 2 * yIdx[ay] + 1
      var gx2 = 2 * xIdx[bx] + 1
      var gy2 = 2 * yIdx[by] + 1
      if gx1 > gx2: swap(gx1, gx2)
      if gy1 > gy2: swap(gy1, gy2)
      let total = P[gy2][gx2]
      let left = if gx1 > 0: P[gy2][gx1 - 1] else: 0
      let up = if gy1 > 0: P[gy1 - 1][gx2] else: 0
      let diag = if gx1 > 0 and gy1 > 0: P[gy1 - 1][gx1 - 1] else: 0
      let valid = total - left - up + diag
      if valid == area:
        maxArea = area

  echo "Largest valid area: ", maxArea

main()
