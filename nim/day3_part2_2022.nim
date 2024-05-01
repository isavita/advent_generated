import std/[sequtils, strutils, sets]

proc priority(c: char): int =
  if c.isLowerAscii:
    return ord(c) - ord('a') + 1
  else:
    return ord(c) - ord('A') + 27

proc part1(input: seq[string]): int =
  for line in input:
    let mid = line.len div 2
    let first = toHashSet(line[0..<mid])
    let second = toHashSet(line[mid..<line.len])
    let common = first * second
    result += priority(common.toSeq[0])

proc part2(input: seq[string]): int =
  for i in countup(0, input.len - 1, 3):
    let group = input[i..<i+3]
    var badge: HashSet[char] = toHashSet(group[0])
    for j in 1..<group.len:
      badge = badge * toHashSet(group[j])
    result += priority(badge.toSeq[0])

when isMainModule:
  let input = readFile("input.txt").strip.splitLines
  echo "Part 1: ", part1(input)
  echo "Part 2: ", part2(input)