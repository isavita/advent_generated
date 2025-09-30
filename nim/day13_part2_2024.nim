
import std/[strutils, os]

type Machine = object
  ax, ay, bx, by, px, py: int64

proc parseLine(s: string): tuple[x, y: int64] =
  let parts = s.split(',')
  result.x = parts[0].strip.replace("X+", "").replace("X=", "").parseBiggestInt()
  result.y = parts[1].strip.replace("Y+", "").replace("Y=", "").parseBiggestInt()

proc parseMachine(lines: openArray[string]): Machine =
  for l in lines:
    let line = l.strip
    if line.startsWith("Button A:"):
      let t = parseLine(line[9..^1])
      result.ax = t.x; result.ay = t.y
    elif line.startsWith("Button B:"):
      let t = parseLine(line[9..^1])
      result.bx = t.x; result.by = t.y
    elif line.startsWith("Prize:"):
      let t = parseLine(line[6..^1])
      result.px = t.x; result.py = t.y

proc solveMachine(m: Machine): int64 =
  let D = m.ax * m.by - m.ay * m.bx
  if D == 0: return -1
  let numA = m.px * m.by - m.py * m.bx
  let numB = -m.px * m.ay + m.py * m.ax
  if numA mod D != 0 or numB mod D != 0: return -1
  let a = numA div D
  let b = numB div D
  if a < 0 or b < 0: return -1
  3 * a + b

proc main() =
  const offset = 10000000000000'i64
  var machines: seq[Machine]
  var lines: seq[string]
  for line in "input.txt".lines:
    let l = line.strip
    if l.len == 0:
      if lines.len > 0:
        machines.add parseMachine(lines)
        lines = @[]
    else:
      lines.add l
  if lines.len > 0:
    machines.add parseMachine(lines)

  for m in machines.mitems:
    m.px += offset
    m.py += offset

  var results: seq[int64]
  for m in machines:
    let c = solveMachine(m)
    if c >= 0: results.add c

  if results.len == 0:
    echo "0 0"
    return
  var sum = 0'i64
  for c in results: sum += c
  echo results.len, " ", sum

when isMainModule: main()
