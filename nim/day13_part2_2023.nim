
import std/[strutils, sequtils, bitops]

type
  Mirror = object
    rows, cols: seq[int]

proc parseMirror(mirrorStr: seq[string]): Mirror =
  result.rows = newSeq[int](mirrorStr.len)
  result.cols = newSeq[int](mirrorStr[0].len)

  for y, line in mirrorStr:
    for x, char in line:
      result.rows[y] = result.rows[y] shl 1
      result.cols[x] = result.cols[x] shl 1
      if char == '#':
        result.rows[y].inc
        result.cols[x].inc

proc getMirrorAxisWithOneSmudge(lines: seq[int]): int =
  for i in 1 ..< lines.len:
    var 
      isMirror = true
      numSmudges = 0

    for j in 0 ..< min(i, lines.len - i):
      if lines[i-1-j] != lines[i+j]:
        if numSmudges > 0:
          isMirror = false
          break
        
        let dif = lines[i-1-j] xor lines[i+j]
        if dif.countSetBits == 1:
          numSmudges.inc
        else:
          isMirror = false
          break

    if isMirror and numSmudges == 1:
      return i

  return 0

proc solve(input: seq[string]): int =
  var 
    mirrors: seq[Mirror]
    mirrorStr: seq[string]

  for line in input:
    if line == "":
      mirrors.add parseMirror(mirrorStr)
      mirrorStr.setLen(0)
    else:
      mirrorStr.add line
  
  mirrors.add parseMirror(mirrorStr)

  for mirror in mirrors:
    result += getMirrorAxisWithOneSmudge(mirror.cols)
    result += getMirrorAxisWithOneSmudge(mirror.rows) * 100

proc main() =
  let input = readFile("input.txt").strip.splitLines
  echo solve(input)

main()
