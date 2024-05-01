import strutils, sequtils, sets

proc firstNUnique(s: string, n: int): int =
  for i in n..<s.len:
    let window = s[i-n..<i]
    let windowSet = toSeq(window.items).toHashSet
    if windowSet.len == n:
      return i
  return -1

let input = readFile("input.txt").strip
echo firstNUnique(input, 4)