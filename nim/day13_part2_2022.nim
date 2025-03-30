
import std/json
import std/strutils
import std/sequtils
import std/algorithm
import system

proc comparePackets(a, b: JsonNode): int =
  if a.kind == JInt and b.kind == JInt:
    let valA = a.getInt()
    let valB = b.getInt()
    if valA < valB: return -1
    if valA > valB: return 1
    return 0
  elif a.kind == JInt:
    let tempA = newJArray()
    tempA.add(a)
    return comparePackets(tempA, b)
  elif b.kind == JInt:
    let tempB = newJArray()
    tempB.add(b)
    return comparePackets(a, tempB)
  elif a.kind == JArray and b.kind == JArray:
    let lenA = a.len
    let lenB = b.len
    for i in 0 ..< min(lenA, lenB):
      let cmp = comparePackets(a.elems[i], b.elems[i])
      if cmp != 0:
        return cmp
    if lenA < lenB: return -1
    if lenA > lenB: return 1
    return 0
  else:
    # Should ideally not happen with valid input based on problem description
    # but handles unexpected types defensively.
    # Assuming arrays and ints are the only possibilities based on context.
    # Might need adjustment if other JSON types are valid packets.
    # Returning 0 might be problematic, depending on sort stability needs.
    # Raising an error might be safer if input validity isn't guaranteed.
    return 0 # Or raise newException(ValueError, "Invalid packet types")


proc main() =
  let s = readFile("input.txt")
  var packets: seq[JsonNode] = @[]

  for pairStr in s.strip.split("\n\n"):
    let lines = pairStr.split('\n')
    if lines.len >= 1 and lines[0].len > 0 :
        packets.add(parseJson(lines[0]))
    if lines.len >= 2 and lines[1].len > 0 :
       packets.add(parseJson(lines[1]))

  let divider1 = parseJson("[[2]]")
  let divider2 = parseJson("[[6]]")
  packets.add(divider1)
  packets.add(divider2)

  packets.sort(comparePackets)

  var divider1Pos = -1
  var divider2Pos = -1

  for i, p in packets:
    if p == divider1:
      divider1Pos = i + 1
    if p == divider2:
      divider2Pos = i + 1

  echo divider1Pos * divider2Pos

when isMainModule:
  main()
