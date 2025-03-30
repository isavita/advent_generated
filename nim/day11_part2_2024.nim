
import strutils, tables, sequtils, os

proc trimLeadingZeros(s: string): string =
  var i = 0
  while i < s.len - 1 and s[i] == '0':
    inc i
  if i >= s.len and s.len > 0 and s[0] == '0': # Handle all zeros or single "0"
     return "0"
  if i > 0 and i == s.len: # Handle empty string case if it occurred somehow (shouldn't here)
     return "0"
  result = s[i .. ^1]
  if result.len == 0 and s.len > 0: # Original was "0..."
    return "0"
  elif result.len == 0: # Original was ""
    return "0"


proc splitStone(s: string): tuple[left: string, right: string] =
  let mid = s.len div 2
  let leftRaw = s[0 ..< mid]
  let rightRaw = s[mid .. ^1]
  let left = trimLeadingZeros(leftRaw)
  let right = trimLeadingZeros(rightRaw)
  # trimLeadingZeros now handles the "or '0'" logic implicitely for all-zero strings
  result = (left: left, right: right)

proc multiplyBy2024(s: string): string =
  if s == "0": return "0"
  var num: seq[int]
  for c in s:
    num.add(parseInt($c))

  let multiplier = [2, 0, 2, 4]
  var res = newSeq[int](num.len + multiplier.len) # Initialized to 0

  for i in countdown(num.len - 1, 0):
    var carry = 0
    for j in countdown(multiplier.len - 1, 0):
      let product = num[i] * multiplier[j] + res[i + j + 1] + carry
      res[i + j + 1] = product mod 10
      carry = product div 10
    res[i] += carry

  var start = 0
  while start < res.len - 1 and res[start] == 0:
    inc start

  if start == res.len : return "0" # Should not happen if input s != "0"

  result = res[start .. ^1].mapIt($it).join("")


proc solve() =
  var stonesMap = initTable[string, int]()
  try:
    let content = readFile("input.txt")
    let stones = content.strip().split()
    for stone in stones:
      stonesMap[stone] = stonesMap.getOrDefault(stone) + 1
  except IOError:
    echo "Error: input.txt not found."
    quit(1)

  let steps = 75
  for _ in 0 ..< steps:
    var newStonesMap = initTable[string, int]()
    for stone, count in stonesMap.pairs:
      if stone == "0":
        newStonesMap["1"] = newStonesMap.getOrDefault("1") + count
      elif stone.len mod 2 == 0:
        let (left, right) = splitStone(stone)
        newStonesMap[left] = newStonesMap.getOrDefault(left) + count
        newStonesMap[right] = newStonesMap.getOrDefault(right) + count
      else: # odd length
        let newStone = multiplyBy2024(stone)
        newStonesMap[newStone] = newStonesMap.getOrDefault(newStone) + count
    stonesMap = newStonesMap

  var totalStones = 0
  for count in stonesMap.values:
    totalStones += count

  echo totalStones

when isMainModule:
  solve()
