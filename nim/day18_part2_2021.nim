
import strutils, options, sequtils, math

type
  SnailNumberObj = object
    value: Option[int]
    left, right: SnailNumberRef # Forward declaration
  SnailNumberRef = ref SnailNumberObj

proc newRegular(val: int): SnailNumberRef =
  new result
  result.value = some(val)

proc newPair(l, r: SnailNumberRef): SnailNumberRef =
  new result
  result.left = l
  result.right = r

proc isRegular(n: SnailNumberRef): bool =
  n.value.isSome

proc addLeft(n: SnailNumberRef; val: int)
proc addRight(n: SnailNumberRef; val: int)

proc explode(n: SnailNumberRef; depth: int): tuple[exploded: bool, leftAdd: int, rightAdd: int] =
  if n.isRegular:
    return (false, 0, 0)

  if depth == 4:
    let leftVal = n.left.value.get
    let rightVal = n.right.value.get
    # Become regular number 0
    n.value = some(0)
    n.left = nil
    n.right = nil
    return (true, leftVal, rightVal)

  var exploded: bool
  var leftAdd, rightAdd: int

  if n.left != nil:
    (exploded, leftAdd, rightAdd) = n.left.explode(depth + 1)
    if exploded:
      if rightAdd > 0 and n.right != nil:
        n.right.addLeft(rightAdd)
      return (true, leftAdd, 0) # Right value handled or not needed further up

  if n.right != nil:
    (exploded, leftAdd, rightAdd) = n.right.explode(depth + 1)
    if exploded:
      if leftAdd > 0 and n.left != nil:
        n.left.addRight(leftAdd)
      return (true, 0, rightAdd) # Left value handled or not needed further up

  return (false, 0, 0)

proc addLeft(n: SnailNumberRef; val: int) =
  if n.isRegular:
    n.value = some(n.value.get + val)
  elif n.left != nil:
     n.left.addLeft(val)
  # Implicitly handles case where only right exists, which shouldn't happen
  # after an explosion needing left add propagation. But for completeness:
  # elif n.right != nil:
  #    n.right.addLeft(val)


proc addRight(n: SnailNumberRef; val: int) =
  if n.isRegular:
    n.value = some(n.value.get + val)
  elif n.right != nil:
     n.right.addRight(val)
  elif n.left != nil:
     n.left.addRight(val)


proc split(n: SnailNumberRef): bool =
  if n.isRegular:
    if n.value.get >= 10:
      let val = n.value.get
      n.left = newRegular(val div 2)
      # Ceil equivalent for integer division: (val + 1) div 2
      n.right = newRegular((val + 1) div 2)
      n.value = none[int]()
      return true
    return false
  else:
    if n.left != nil and n.left.split():
      return true
    if n.right != nil and n.right.split():
      return true
    return false

proc reduce(n: SnailNumberRef): SnailNumberRef =
  while true:
    var (exploded, _, _) = n.explode(0)
    if exploded:
      continue
    if not n.split():
      break
  return n

proc add(a, b: SnailNumberRef): SnailNumberRef =
  let newNum = newPair(a, b)
  return newNum.reduce()

proc magnitude(n: SnailNumberRef): int =
  if n.isRegular:
    return n.value.get
  else:
    var leftMag = 0
    var rightMag = 0
    if n.left != nil: leftMag = n.left.magnitude
    if n.right != nil: rightMag = n.right.magnitude
    return 3 * leftMag + 2 * rightMag

proc deepCopy(n: SnailNumberRef): SnailNumberRef =
  if n.isRegular:
    result = newRegular(n.value.get)
  else:
    var leftCopy: SnailNumberRef = nil
    var rightCopy: SnailNumberRef = nil
    if n.left != nil: leftCopy = n.left.deepCopy
    if n.right != nil: rightCopy = n.right.deepCopy
    result = newPair(leftCopy, rightCopy)

proc parseSnailNumber(s: var string): SnailNumberRef =
  s = s.strip()
  if s[0] != '[':
    return newRegular(parseInt(s))

  var balance = 0
  var splitIndex = -1
  for i in 1 ..< s.len - 1:
    case s[i]
    of '[': balance += 1
    of ']': balance -= 1
    of ',':
      if balance == 0:
        splitIndex = i
        break
    else: discard

  var leftStr = s[1 ..< splitIndex]
  var rightStr = s[splitIndex + 1 ..< s.len - 1]

  let left = parseSnailNumber(leftStr)
  let right = parseSnailNumber(rightStr)
  return newPair(left, right)

proc main() =
  var snailNumbers: seq[SnailNumberRef] = @[]
  try:
    for line in lines("input.txt"):
      if line.len > 0:
        var lineCopy = line # Need mutable copy for parseSnailNumber
        snailNumbers.add(parseSnailNumber(lineCopy))
  except IOError:
    echo "Error reading input.txt"
    return

  if snailNumbers.len == 0:
    echo "No snailfish numbers found in the file."
    return

  var largestMagnitude = 0
  for i in 0 ..< snailNumbers.len:
    for j in 0 ..< snailNumbers.len:
      if i == j: continue

      # Need deep copies as add() modifies the numbers
      let sum1 = add(snailNumbers[i].deepCopy, snailNumbers[j].deepCopy).magnitude
      largestMagnitude = max(largestMagnitude, sum1)
      
      # Don't strictly need sum2 if we iterate through all pairs (i,j) and (j,i)
      # let sum2 = add(snailNumbers[j].deepCopy, snailNumbers[i].deepCopy).magnitude
      # largestMagnitude = max(largestMagnitude, sum2)


  echo largestMagnitude

when isMainModule:
  main()
