
import os, strutils, sequtils, collections/deques

const dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]

proc bfs(start: (int, int), h, w: int, walls: seq[seq[bool]]): seq[seq[int]] =
  result = newSeqWith(h, newSeqWith(w, -1))
  if start[0] < 0 or start[0] >= h or start[1] < 0 or start[1] >= w or walls[start[0]][start[1]]:
    return # Invalid start or start on wall

  result[start[0]][start[1]] = 0
  var q = initDeque[(int, int)]()
  q.addLast(start)

  while q.len > 0:
    let (r, c) = q.popFirst()
    for (dr, dc) in dirs:
      let nr = r + dr
      let nc = c + dc

      if nr >= 0 and nr < h and nc >= 0 and nc < w:
        if not walls[nr][nc] and result[nr][nc] == -1:
          result[nr][nc] = result[r][c] + 1
          q.addLast((nr, nc))

proc isTrack(r, c: int, h, w: int, walls: seq[seq[bool]]): bool =
  result = r >= 0 and r < h and c >= 0 and c < w and not walls[r][c]

proc main() =
  let content = readFile("input.txt")
  let grid = content.strip.splitLines
  if grid.len == 0:
    echo 0
    return

  let h = grid.len
  let w = grid[0].len

  var S, E: (int, int) = (-1, -1)
  var walls = newSeqWith(h, newSeq[bool](w))
  var trackCells = newSeq[(int, int)]()

  for r in 0..<h:
    for c in 0..<w:
      case grid[r][c]
      of '#': walls[r][c] = true
      of 'S': S = (r, c); trackCells.add((r, c))
      of 'E': E = (r, c); trackCells.add((r, c))
      else: trackCells.add((r, c)) # Assume others are track cells

  if S[0] == -1 or E[0] == -1: # S or E not found
      echo 0 # Or handle error appropriately
      return

  let distFromS = bfs(S, h, w, walls)
  let distFromE = bfs(E, h, w, walls)

  let normalCost = distFromS[E[0]][E[1]]
  if normalCost == -1:
    echo 0
    return

  var possibleCheats = 0
  let costThreshold = normalCost - 100 # Precompute threshold for saving >= 100

  for startPos in trackCells:
    let sd = distFromS[startPos[0]][startPos[1]]
    if sd == -1: continue

    for (dr1, dc1) in dirs:
      let m1r = startPos[0] + dr1
      let m1c = startPos[1] + dc1
      # No need to check m1 bounds explicitly, m2 check covers it

      for (dr2, dc2) in dirs:
        let m2r = m1r + dr2
        let m2c = m1c + dc2

        if isTrack(m2r, m2c, h, w, walls):
          let ed = distFromE[m2r][m2c]
          if ed == -1: continue

          # Check if saving >= 100  <=> sd + 2 + ed <= normalCost - 100
          if sd + ed + 2 <= costThreshold:
            inc possibleCheats

  echo possibleCheats

when isMainModule:
  main()
