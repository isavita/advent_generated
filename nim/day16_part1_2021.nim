import strutils, sequtils

proc hexToBin(hex: string): string =
  result = ""
  for c in hex:
    case c
    of '0': result.add "0000"
    of '1': result.add "0001"
    of '2': result.add "0010"
    of '3': result.add "0011"
    of '4': result.add "0100"
    of '5': result.add "0101"
    of '6': result.add "0110"
    of '7': result.add "0111"
    of '8': result.add "1000"
    of '9': result.add "1001"
    of 'A': result.add "1010"
    of 'B': result.add "1011"
    of 'C': result.add "1100"
    of 'D': result.add "1101"
    of 'E': result.add "1110"
    of 'F': result.add "1111"
    else: discard

proc binToInt(bin: string): int =
  result = 0
  for i, c in bin:
    if c == '1':
      result += 1 shl (bin.len - i - 1)

proc parsePacket(bin: string, pos: var int): int =
  let version = binToInt(bin[pos..pos+2])
  pos += 3
  let typeID = binToInt(bin[pos..pos+2])
  pos += 3
  var versionSum = version
  if typeID == 4:
    var literal = ""
    while true:
      let group = bin[pos]
      pos += 1
      literal.add bin[pos..pos+3]
      pos += 4
      if group == '0':
        break
  else:
    let lengthTypeID = bin[pos]
    pos += 1
    if lengthTypeID == '0':
      let subPacketLength = binToInt(bin[pos..pos+14])
      pos += 15
      let endPos = pos + subPacketLength
      while pos < endPos:
        versionSum += parsePacket(bin, pos)
    else:
      let subPacketCount = binToInt(bin[pos..pos+10])
      pos += 11
      for _ in 0..<subPacketCount:
        versionSum += parsePacket(bin, pos)
  return versionSum

when isMainModule:
  let input = readFile("input.txt").strip
  let bin = hexToBin(input)
  var pos = 0
  let versionSum = parsePacket(bin, pos)
  echo versionSum