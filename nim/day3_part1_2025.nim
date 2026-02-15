
import strutils

proc maxJoltage(s: string): int =
  for t in countdown(9, 1):
    let idx = s.find($t)
    if idx != -1 and idx < s.len - 1:
      var maxU = 0
      for i in idx + 1 .. s.high:
        let d = ord(s[i]) - ord('0')
        if d > maxU: maxU = d
      return t * 10 + maxU
  return 0

proc main() =
  try:
    let data = readFile("input.txt").splitLines()
    var total = 0
    for line in data:
      let trimmedLine = line.strip()
      if trimmedLine.len > 0:
        total += maxJoltage(trimmedLine)
    echo total
  except IOError:
    echo "Error reading file 'input.txt'"
    quit(1)

main()
