import strutils

proc isValid(preamble: seq[int], target: int): bool =
  for i in 0..<preamble.len:
    for j in i+1..<preamble.len:
      if preamble[i] + preamble[j] == target:
        return true
  return false

proc findInvalidNumber(filename: string, preambleSize: int): int =
  let file = readFile(filename)
  var numbers: seq[int] = @[]
  for line in file.splitLines():
    numbers.add(parseInt(line))
  for i in preambleSize..<numbers.len:
    let preamble = numbers[i-preambleSize..i-1]
    if not isValid(preamble, numbers[i]):
      return numbers[i]
  return -1

let invalidNumber = findInvalidNumber("input.txt", 25)
echo invalidNumber