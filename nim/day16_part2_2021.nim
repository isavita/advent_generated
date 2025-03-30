
import strutils, math, sequtils, streams

proc hexToBin(hexStr: string): string =
  result = ""
  for h in hexStr:
    let val = parseHexInt($h)
    var binPart = toBin(val, 4)
    result.add(binPart)

proc binToInt(binSlice: string): int64 =
  result = 0
  for bit in binSlice:
    result = (result shl 1) or (if bit == '1': 1 else: 0)

type PacketResult = tuple[version: int, newIndex: int, value: int64]

proc parsePacket(binStr: string, idx: var int): PacketResult =
  let version = binToInt(binStr[idx ..< idx + 3]).int
  let typeId = binToInt(binStr[idx + 3 ..< idx + 6]).int
  idx += 6

  if typeId == 4:
    var literalValue: int64 = 0
    while binStr[idx] == '1':
      literalValue = (literalValue shl 4) or binToInt(binStr[idx + 1 ..< idx + 5])
      idx += 5
    literalValue = (literalValue shl 4) or binToInt(binStr[idx + 1 ..< idx + 5])
    idx += 5
    return (version, idx, literalValue)
  else:
    let lengthTypeId = binStr[idx]
    idx += 1
    var subValues: seq[int64] = @[]

    if lengthTypeId == '0':
      var subPacketLength = binToInt(binStr[idx ..< idx + 15]).int
      idx += 15
      let targetIdx = idx + subPacketLength
      while idx < targetIdx:
        let subResult = parsePacket(binStr, idx)
        subValues.add(subResult.value)
    else:
      var numSubPackets = binToInt(binStr[idx ..< idx + 11]).int
      idx += 11
      for _ in 0 ..< numSubPackets:
        let subResult = parsePacket(binStr, idx)
        subValues.add(subResult.value)

    var resultValue: int64 = 0
    case typeId
    of 0: # Sum
      resultValue = subValues.foldl(a + b)
    of 1: # Product
      resultValue = subValues.foldl(a * b, 1'i64)
    of 2: # Minimum
      resultValue = subValues[0]
      for i in 1 ..< subValues.len:
        resultValue = min(resultValue, subValues[i])
    of 3: # Maximum
      resultValue = subValues[0]
      for i in 1 ..< subValues.len:
        resultValue = max(resultValue, subValues[i])
    of 5: # Greater Than
      resultValue = if subValues[0] > subValues[1]: 1 else: 0
    of 6: # Less Than
      resultValue = if subValues[0] < subValues[1]: 1 else: 0
    of 7: # Equal To
      resultValue = if subValues[0] == subValues[1]: 1 else: 0
    else:
      raise newException(ValueError, "Unknown typeID: " & $typeId)

    return (version, idx, resultValue)

when isMainModule:
  let hexStr = readFile("input.txt").strip()
  let binStr = hexToBin(hexStr)
  var idx = 0
  let result = parsePacket(binStr, idx)
  echo result.value
