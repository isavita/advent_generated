
import std/[strutils, parseutils, sequtils, math, streams]

type
  SnailNumber = ref object
    value: int
    left, right: SnailNumber

proc isRegular(sn: SnailNumber): bool =
  sn.left.isNil and sn.right.isNil

proc newRegular(value: int): SnailNumber =
  SnailNumber(value: value)

proc newPair(left, right: SnailNumber): SnailNumber =
  SnailNumber(left: left, right: right)

proc addLeft(sn: var SnailNumber, value: int) =
  if sn.isRegular:
    sn.value += value
  else:
    sn.left.addLeft(value)

proc addRight(sn: var SnailNumber, value: int) =
  if sn.isRegular:
    sn.value += value
  else:
    sn.right.addRight(value)

proc explode(sn: var SnailNumber, depth: int): tuple[exploded: bool, left, right: int] =
  if sn.isRegular:
    return (false, 0, 0)

  if depth == 4:
    result = (true, sn.left.value, sn.right.value)
    sn.left = nil
    sn.right = nil
    sn.value = 0
    return

  var (exploded, leftVal, rightVal) = sn.left.explode(depth + 1)
  if exploded:
    if rightVal > 0 and sn.right != nil:
      sn.right.addLeft(rightVal)
    return (true, leftVal, 0)

  (exploded, leftVal, rightVal) = sn.right.explode(depth + 1)
  if exploded:
    if leftVal > 0 and sn.left != nil:
      sn.left.addRight(leftVal)
    return (true, 0, rightVal)

  return (false, 0, 0)

proc split(sn: var SnailNumber): bool =
  if sn.isRegular:
    if sn.value >= 10:
      sn.left = newRegular(sn.value div 2)
      sn.right = newRegular((sn.value + 1) div 2)
      sn.value = -1
      return true
    return false

  result = sn.left.split() or sn.right.split()

proc reduce(sn: var SnailNumber) =
  while true:
    var exploded = sn.explode(0).exploded
    if exploded:
      continue
    if not sn.split():
      break

proc add(a: var SnailNumber, b: SnailNumber): SnailNumber =
  result = newPair(a, b)
  result.reduce()

proc magnitude(sn: SnailNumber): int =
  if sn.isRegular:
    return sn.value
  3 * sn.left.magnitude() + 2 * sn.right.magnitude()

proc parseSnailNumber(s: string): SnailNumber =
  if s[0] != '[':
    return newRegular(parseInt(s))

  var 
    balance = 0
    splitIndex = 0

  for i in 1..<s.high:
    case s[i]
    of '[': inc(balance)
    of ']': dec(balance)
    of ',':
      if balance == 0:
        splitIndex = i
        break
    else: discard

  let 
    left = parseSnailNumber(s[1..<splitIndex])
    right = parseSnailNumber(s[splitIndex+1..^2])
  
  result = newPair(left, right)

proc main() =
  var file = newFileStream("input.txt", fmRead)
  if file.isNil:
    quit("Cannot open input file")

  var snailNumbers: seq[SnailNumber]
  var line = ""
  while file.readLine(line):
    snailNumbers.add(parseSnailNumber(line))

  file.close()

  if snailNumbers.len == 0:
    quit("No snailfish numbers found")

  var result = snailNumbers[0]
  for i in 1..<snailNumbers.len:
    result = result.add(snailNumbers[i])

  echo result.magnitude()

main()
