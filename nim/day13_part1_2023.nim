
import strutils, sequtils, math, os

type Mirror = object
  rows: seq[int]
  cols: seq[int]

proc parseMirror(mirrorStr: seq[string]): Mirror =
  if mirrorStr.len == 0 or mirrorStr[0].len == 0:
    return Mirror(rows: newSeq[int](0), cols: newSeq[int](0))

  let height = mirrorStr.len
  let width = mirrorStr[0].len
  result.rows = newSeq[int](height)
  result.cols = newSeq[int](width)

  for y, line in mirrorStr:
    for x, char in line:
      result.rows[y] = result.rows[y] shl 1
      result.cols[x] = result.cols[x] shl 1
      if char == '#':
        result.rows[y] += 1
        result.cols[x] += 1

proc parseInput(input: seq[string]): seq[Mirror] =
  var mirrorStr: seq[string] = @[]
  for line in input:
    if line.len == 0:
      if mirrorStr.len > 0:
        result.add(parseMirror(mirrorStr))
        mirrorStr = @[]
    else:
      mirrorStr.add(line)
  
  if mirrorStr.len > 0:
      result.add(parseMirror(mirrorStr))

proc getMirrorAxis(lines: seq[int]): int =
  for i in 1 ..< lines.len:
    var isMirror = true
    let checkRange = min(i, lines.len - i)
    for j in 0 ..< checkRange:
      if lines[i - 1 - j] != lines[i + j]:
        isMirror = false
        break
    if isMirror:
      return i
  return 0

# Note: The original Python solve function doesn't use the smudge logic,
# so this function is included for completeness but not called by solve.
proc getMirrorAxisWithOneSmudge(lines: seq[int]): int =
  for i in 1 ..< lines.len:
    var isMirror = true
    var numSmudges = 0
    let checkRange = min(i, lines.len - i)
    for j in 0 ..< checkRange:
        if lines[i-1-j] != lines[i+j]:
            if numSmudges > 0:
                isMirror = false
                break
            else:
                let dif = lines[i-1-j] xor lines[i+j]
                # Check if dif is a power of 2 (only one bit set)
                let isOnlyOneSmudge = (dif != 0) and ((dif and (dif - 1)) == 0)
                if isOnlyOneSmudge:
                    numSmudges += 1
                else:
                    isMirror = false
                    break
        
    if isMirror and numSmudges == 1:
      return i
    
  return 0

proc solve(input: seq[string]): int =
  let mirrors = parseInput(input)
  for mirror in mirrors:
    result += getMirrorAxis(mirror.cols)
    result += getMirrorAxis(mirror.rows) * 100

proc main() =
  if fileExists("input.txt"):
    let input = readFile("input.txt").strip.splitLines
    echo solve(input)
  else:
    echo "Error: input.txt not found."

when isMainModule:
  main()
