
import strutils, algorithm, sets, os, sequtils

proc containsAll(larger, smaller: string): bool =
  if smaller.len > larger.len: return false
  let largerSet = larger.toHashSet
  for c in smaller:
    if c notin largerSet:
      return false
  return true

proc solve(): int =
  let inputData = readFile("input.txt").strip()
  result = 0

  for line in inputData.splitLines:
    let parts = line.split({' ', '|'}).filterIt(it.len > 0)
    if parts.len != 14:
      raise newException(ValueError, "Input line does not contain 14 parts")

    var sortedParts: array[14, string]
    for i, p in parts:
      sortedParts[i] = sorted(p).join("")

    let signals = sortedParts[0..9]
    let outputs = sortedParts[10..13]

    var digitPatterns: array[10, string]
    var remainingSignals = signals.toSeq()

    # Identify 1, 4, 7, 8 by unique lengths
    var nextRemainingSignals: seq[string] = @[]
    for signal in remainingSignals:
      case signal.len
      of 2: digitPatterns[1] = signal
      of 4: digitPatterns[4] = signal
      of 3: digitPatterns[7] = signal
      of 7: digitPatterns[8] = signal
      else: nextRemainingSignals.add(signal)
    remainingSignals = nextRemainingSignals

    # Identify 0, 3, 9 (contain segments of 1)
    var zeroThreeNineCandidates: seq[string] = @[]
    nextRemainingSignals = @[]
    for signal in remainingSignals:
      if containsAll(signal, digitPatterns[1]):
        zeroThreeNineCandidates.add(signal)
      else:
        nextRemainingSignals.add(signal)
    remainingSignals = nextRemainingSignals

    if zeroThreeNineCandidates.len != 3:
      raise newException(ValueError, "Could not isolate 0, 3, 9 candidates")

    var zeroNineCandidates: seq[string] = @[]
    for signal in zeroThreeNineCandidates:
      if signal.len == 5:
        digitPatterns[3] = signal
      else:
        zeroNineCandidates.add(signal)

    if zeroNineCandidates.len != 2:
        raise newException(ValueError, "Could not isolate 0, 9 candidates")

    if containsAll(zeroNineCandidates[0], digitPatterns[4]):
      digitPatterns[9] = zeroNineCandidates[0]
      digitPatterns[0] = zeroNineCandidates[1]
    else:
      digitPatterns[9] = zeroNineCandidates[1]
      digitPatterns[0] = zeroNineCandidates[0]


    # Identify 6 (len 6)
    var indexToRemove = -1
    for i, signal in remainingSignals:
      if signal.len == 6:
        digitPatterns[6] = signal
        indexToRemove = i
        break
    if indexToRemove != -1:
      remainingSignals.delete(indexToRemove)
    else:
       raise newException(ValueError, "Could not find digit 6")


    # Identify 5 (contained within 9)
    indexToRemove = -1
    for i, signal in remainingSignals:
        if containsAll(digitPatterns[9], signal):
            digitPatterns[5] = signal
            indexToRemove = i
            break
    if indexToRemove != -1:
        remainingSignals.delete(indexToRemove)
    else:
        raise newException(ValueError, "Could not find digit 5")


    # Remaining is 2
    if remainingSignals.len != 1:
        raise newException(ValueError, "Error isolating digit 2")
    digitPatterns[2] = remainingSignals[0]

    # Decode output
    var currentVal = 0
    for output in outputs:
      for digit, pattern in digitPatterns:
        if output == pattern:
          currentVal = currentVal * 10 + digit
          break
    result += currentVal

proc main() =
  let answer = solve()
  echo answer

when isMainModule:
  main()
