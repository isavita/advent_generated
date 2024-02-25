
import strutils

var
  file = open("input.txt")
  lines = file.readAll.splitLines
  checksum = 0

for line in lines:
  var
    nums = line.split
    minVal = parseInt(nums[0])
    maxVal = parseInt(nums[0])

  for numStr in nums:
    let num = parseInt(numStr)
    if num < minVal:
      minVal = num
    if num > maxVal:
      maxVal = num

  checksum += maxVal - minVal

echo checksum
