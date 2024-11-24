
import strutils, os

proc findFirstAndLastDigit(line: string): tuple[firstDigit, lastDigit: int] =
  let digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  var firstDigit, lastDigit: int = -1

  for i, char in line:
    if char in '0'..'9':
      let digit = ord(char) - ord('0')
      if firstDigit == -1:
        firstDigit = digit
      lastDigit = digit
    else:
      for j, digitStr in digits:
        if line[i..^1].startsWith(digitStr):
          if firstDigit == -1:
            firstDigit = j
          lastDigit = j
          break

  return (firstDigit, lastDigit)

var sum = 0
for line in lines("input.txt"):
  let (firstDigit, lastDigit) = findFirstAndLastDigit(line)
  sum += 10 * firstDigit + lastDigit

echo sum
