
import std/[strutils, sequtils, algorithm, sets, tables, strformat, os]

type
  Point = tuple[r, c: int]
  Piece = object
    pts: seq[Point]
    n: int
  Region = object
    w, h: int
    counts: seq[int]

proc normalize(p: Piece): Piece =
  if p.n == 0: return p
  var minR = int.high; var minC = int.high
  for pt in p.pts:
    minR = min(minR, pt.r)
    minC = min(minC, pt.c)
  result.n = p.n
  result.pts = newSeq[Point](p.n)
  for i, pt in p.pts:
    result.pts[i] = (pt.r - minR, pt.c - minC)
  result.pts.sort(proc (a, b: Point): int =
    if a.r != b.r: a.r - b.r else: a.c - b.c)

proc rotate(p: Piece): Piece =
  result.n = p.n
  result.pts = newSeq[Point](p.n)
  for i, pt in p.pts:
    result.pts[i] = (pt.c, -pt.r)

proc flip(p: Piece): Piece =
  result.n = p.n
  result.pts = newSeq[Point](p.n)
  for i, pt in p.pts:
    result.pts[i] = (pt.r, -pt.c)

proc equal(a, b: Piece): bool =
  if a.n != b.n: return false
  for i in 0..<a.n:
    if a.pts[i] != b.pts[i]: return false
  true

proc generateVariations(base: Piece): seq[Piece] =
  var uniq: seq[Piece]
  var cur = base
  for _ in 0..<4:
    let n = normalize(cur)
    if not uniq.anyIt(equal(it, n)): uniq.add n
    let f = flip(cur)
    let nf = normalize(f)
    if not uniq.anyIt(equal(it, nf)): uniq.add nf
    cur = rotate(cur)
  uniq

proc canPlace(rows, cols: int; grid: seq[byte]; p: Piece; r, c: int): bool =
  for pt in p.pts:
    let nr = r + pt.r
    let nc = c + pt.c
    if nr < 0 or nr >= rows or nc < 0 or nc >= cols: return false
    if grid[nr * cols + nc] != 0: return false
  true

proc place(cols: int; grid: var seq[byte]; p: Piece; r, c: int; v: byte) =
  for pt in p.pts:
    grid[(r + pt.r) * cols + (c + pt.c)] = v

proc checkIslands(rows, cols: int; grid: seq[byte]; counts: seq[int];
                  slackIdx: int; shapes: seq[Piece]): bool =
  var minReal = int.high
  var hasReal = false
  for i, cnt in counts:
    if i != slackIdx and cnt > 0:
      minReal = min(minReal, shapes[i].n)
      hasReal = true
  if not hasReal: return true
  var avail = counts[slackIdx]
  var vis = newSeq[bool](rows * cols)
  var q = newSeq[int](rows * cols)
  for idx in 0..<rows * cols:
    if grid[idx] == 0 and not vis[idx]:
      var qs = 0; var qe = 0
      q[qe] = idx; inc qe; vis[idx] = true
      var size = 0
      while qs < qe:
        let cur = q[qs]; inc qs; inc size
        let r = cur div cols; let c = cur mod cols
        template check(dr, dc: int) =
          let nr = r + dr; let nc = c + dc
          if nr >= 0 and nr < rows and nc >= 0 and nc < cols:
            let n = nr * cols + nc
            if grid[n] == 0 and not vis[n]:
              vis[n] = true; q[qe] = n; inc qe
        check(-1, 0); check(1, 0); check(0, -1); check(0, 1)
      if size < minReal:
        if avail >= size: dec avail, size else: return false
  true

proc solveRec(rows, cols: int; grid: var seq[byte]; counts: var seq[int];
              variations: seq[seq[Piece]]; slackIdx: int; shapes: seq[Piece]): bool =
  var empty = -1
  for i, v in grid:
    if v == 0:
      empty = i
      break
  if empty == -1: return true
  let r = empty div cols
  let c = empty mod cols
  if not checkIslands(rows, cols, grid, counts, slackIdx, shapes): return false
  for id in 0..<counts.len:
    if counts[id] > 0:
      dec counts[id]
      for v in variations[id]:
        if canPlace(rows, cols, grid, v, r, c):
          place(cols, grid, v, r, c, 1)
          if solveRec(rows, cols, grid, counts, variations, slackIdx, shapes):
            return true
          place(cols, grid, v, r, c, 0)
      inc counts[id]
  false

proc main() =
  let lines = readFile("input.txt").splitLines
  var maxId = -1000000
  for l in lines:
    let s = l.strip
    if s.len > 0 and s[^1] == ':':
      let id = parseInt(s[0..^2])
      maxId = max(maxId, id)
  if maxId < 0: maxId = -1
  let arrSize = maxId + 2
  let slackIdx = maxId + 1
  var shapes = newSeq[Piece](arrSize)
  var parsingShapes = true
  var currentId = -1
  var curShape: seq[string]
  var regionLines: seq[string]
  for raw in lines:
    let s = raw.strip
    if s.len == 0: continue
    if 'x' in s and ':' in s: parsingShapes = false
    if parsingShapes:
      if s.len > 0 and s[^1] == ':':
        if currentId != -1 and curShape.len > 0:
          var pts: seq[Point]
          for r, row in curShape:
            for c in 0..<row.len:
              if row[c] == '#': pts.add (r, c)
          shapes[currentId] = normalize(Piece(pts: pts, n: pts.len))
          curShape = @[]
        currentId = parseInt(s[0..^2])
      else:
        curShape.add s
    else:
      regionLines.add s
  if currentId != -1 and curShape.len > 0:
    var pts: seq[Point]
    for r, row in curShape:
      for c in 0..<row.len:
        if row[c] == '#': pts.add (r, c)
    shapes[currentId] = normalize(Piece(pts: pts, n: pts.len))
  shapes[slackIdx] = Piece(pts: @[(0, 0)], n: 1)
  var variations = newSeq[seq[Piece]](arrSize)
  for i in 0..<arrSize:
    if shapes[i].n > 0:
      variations[i] = generateVariations(shapes[i])
  var solved = 0
  for line in regionLines:
    let parts = line.split(':')
    if parts.len == 2:
      let dims = parts[0].strip
      let countsStr = parts[1].strip
      let dimParts = dims.split('x')
      if dimParts.len == 2:
        let wx = parseInt(dimParts[0])
        let h = parseInt(dimParts[1])
        let gridSize = wx * h
        var pieceCounts = newSeq[int](arrSize)
        var totalArea = 0
        let toks = countsStr.splitWhitespace()
        for idx, t in toks:
          if idx >= arrSize - 1: break
          let c = parseInt(t)
          if c > 0:
            pieceCounts[idx] = c
            totalArea += c * shapes[idx].n
        if totalArea <= gridSize:
          let slack = gridSize - totalArea
          if slack > 0: pieceCounts[slackIdx] = slack
          var ids: seq[int]
          for i, cnt in pieceCounts:
            if cnt > 0: ids.add i
          ids.sort(proc (a, b: int): int = shapes[b].n - shapes[a].n)
          var grid = newSeq[byte](gridSize)
          if solveRec(h, wx, grid, pieceCounts, variations, slackIdx, shapes):
            inc solved
  echo fmt"Number of regions that fit all presents: {solved}"

when isMainModule: main()
