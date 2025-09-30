
import strutils, sequtils, algorithm

var
  happiness: array[20, array[20, int]]
  guests: array[20, string]
  numGuests: int

proc findGuestIndex(name: string): int =
  for i in 0..<numGuests:
    if guests[i] == name:
      return i
  guests[numGuests] = name
  result = numGuests
  inc numGuests

proc main =
  let lines = readFile("input.txt").strip.splitLines
  for line in lines:
    let parts = line.splitWhitespace
    var fromName = parts[0]
    var toName = parts[^1][0..^2]
    var value = parseInt(parts[3])
    if parts[2] == "lose":
      value = -value
    let fromIdx = findGuestIndex(fromName)
    let toIdx = findGuestIndex(toName)
    happiness[fromIdx][toIdx] = value

  var indices = toSeq(0..<numGuests)
  var maxHappiness = 0

  while true:
    var sum = 0
    for i in 0..<numGuests:
      let left = (i + numGuests - 1) mod numGuests
      let right = (i + 1) mod numGuests
      sum += happiness[indices[i]][indices[left]] + happiness[indices[i]][indices[right]]
    maxHappiness = max(maxHappiness, sum)
    if not indices.nextPermutation():
      break

  echo maxHappiness

main()
