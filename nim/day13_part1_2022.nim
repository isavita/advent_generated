import os
import strutils
import sequtils

type
  Packet = object
    isInt: bool
    intValue: int
    listValue: seq[Packet]

proc parsePacket(s: string): Packet =
  if s[0] == '[':
    var listValue: seq[Packet] = @[]
    var depth = 0
    var start = 1
    for i in 1..<s.len-1:
      if s[i] == '[':
        inc depth
      elif s[i] == ']':
        dec depth
      elif s[i] == ',' and depth == 0:
        listValue.add(parsePacket(s[start..<i]))
        start = i + 1
    if start < s.len - 1:
      listValue.add(parsePacket(s[start..<s.len-1]))
    result = Packet(isInt: false, listValue: listValue)
  else:
    result = Packet(isInt: true, intValue: parseInt(s))

proc comparePackets(left, right: Packet): int =
  if left.isInt and right.isInt:
    return cmp(left.intValue, right.intValue)
  if not left.isInt and not right.isInt:
    for i in 0..<min(left.listValue.len, right.listValue.len):
      let cmpResult = comparePackets(left.listValue[i], right.listValue[i])
      if cmpResult != 0:
        return cmpResult
    return cmp(left.listValue.len, right.listValue.len)
  if left.isInt:
    return comparePackets(Packet(isInt: false, listValue: @[left]), right)
  return comparePackets(left, Packet(isInt: false, listValue: @[right]))

when isMainModule:
  let file = open("input.txt")
  var index = 1
  var sum = 0
  while not file.endOfFile:
    let left = parsePacket(file.readLine())
    let right = parsePacket(file.readLine())
    if comparePackets(left, right) == -1:
      sum += index
    index += 1
    if not file.endOfFile:
      discard file.readLine()  # Skip blank line
  echo sum