import system

proc hasExactlyTwoOrThree(s: string): (bool, bool) =
  var counts: array[256, int]
  for c in s:
    inc counts[ord(c)]
  var hasTwo = false
  var hasThree = false
  for count in counts:
    if count == 2:
      hasTwo = true
    elif count == 3:
      hasThree = true
  (hasTwo, hasThree)

proc main =
  var hasTwoCount = 0
  var hasThreeCount = 0
  for line in "input.txt".lines:
    let (hasTwo, hasThree) = hasExactlyTwoOrThree(line)
    if hasTwo:
      inc hasTwoCount
    if hasThree:
      inc hasThreeCount
  echo "Checksum: ", hasTwoCount * hasThreeCount

when isMainModule:
  main()