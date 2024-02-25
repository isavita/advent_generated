
import strutils

var validCount = 0

for line in lines("input.txt"):
  let parts = line.split(' ')
  let range = parts[0].split('-')
  let pos1 = parseInt(range[0])
  let pos2 = parseInt(range[1])
  let char = parts[1][0]
  let password = parts[2]

  var count = 0
  if password[pos1 - 1] == char:
    count += 1
  if password[pos2 - 1] == char:
    count += 1

  if count == 1:
    validCount += 1

echo validCount
