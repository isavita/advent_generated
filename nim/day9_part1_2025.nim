
import strutils

proc main() =
  let data = readFile("input.txt").splitLines()
  var xs: seq[int64] = @[]
  var ys: seq[int64] = @[]

  for line in data:
    let trimmed = line.strip()
    if trimmed.len == 0: continue
    let parts = trimmed.split(',')
    if parts.len != 2: continue
    try:
      xs.add(parts[0].strip().parseBiggestInt())
      ys.add(parts[1].strip().parseBiggestInt())
    except ValueError:
      continue

  let n = xs.len
  if n == 0: return

  var maxArea: int64 = 1

  for i in 0..<n:
    let x1 = xs[i]
    let y1 = ys[i]
    for j in (i+1)..<n:
      let width = max(x1, xs[j]) - min(x1, xs[j]) + 1
      let height = max(y1, ys[j]) - min(y1, ys[j]) + 1
      let area = width * height
      if area > maxArea: maxArea = area

  echo maxArea

when isMainModule:
  main()
