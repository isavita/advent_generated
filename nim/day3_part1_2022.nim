import std/[sequtils, strutils, tables, streams]

proc itemPriority(item: char): int =
  if item.isLowerAscii:
    return ord(item) - ord('a') + 1
  else:
    return ord(item) - ord('A') + 27

proc main =
  let file = newFileStream("input.txt", fmRead)
  defer: file.close()

  var sum = 0
  for line in file.lines:
    let half = line.len div 2
    let firstCompartment = line[0..half-1]
    let secondCompartment = line[half..^1]

    var compartmentMap = initTable[char, int]()
    for item in firstCompartment:
      if compartmentMap.hasKey(item):
        compartmentMap[item] = compartmentMap[item] + 1
      else:
        compartmentMap[item] = 1
    for item in secondCompartment:
      if compartmentMap.hasKey(item):
        sum += itemPriority(item)
        break

  echo sum

when isMainModule:
  main()