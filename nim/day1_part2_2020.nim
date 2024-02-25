import strutils, sequtils

proc parseInts(s: seq[string]): seq[int] =
  result = @[]
  for x in s:
    try:
      let num = parseInt(x)
      result.add(num)
    except ValueError:
      # Skip lines that cannot be converted to integers
      continue

proc solvePart1(expenses: seq[int]): int =
  for i in 0..<expenses.high:
    for j in i+1..<expenses.high:
      if expenses[i] + expenses[j] == 2020:
        return expenses[i] * expenses[j]
  return 0

proc solvePart2(expenses: seq[int]): int =
  for i in 0..<expenses.high:
    for j in i+1..<expenses.high:
      for k in j+1..<expenses.high:
        if expenses[i] + expenses[j] + expenses[k] == 2020:
          return expenses[i] * expenses[j] * expenses[k]
  return 0

when isMainModule:
  let input = readFile("input.txt")
  let expenses = parseInts(input.split("\n"))
  echo "Part 1: ", solvePart1(expenses)
  echo "Part 2: ", solvePart2(expenses)
