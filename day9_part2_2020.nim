
import strutils

var input: seq[int] = @[]

for line in lines("input.txt"):
  input.add parseInt(line)

proc findInvalidNumber(nums: seq[int], preambleLen: int): int =
  for i in preambleLen..<len(nums):
    var valid = false
    for j in i - preambleLen..<i:
      for k in j + 1..<i:
        if nums[j] + nums[k] == nums[i]:
          valid = true
    if not valid:
      return nums[i]

let invalidNumber = findInvalidNumber(input, 25)
echo invalidNumber

proc findEncryptionWeakness(nums: seq[int], target: int): int =
  for i in 0..<len(nums):
    var sum = nums[i]
    var j = i + 1
    while sum < target:
      sum += nums[j]
      inc j
    if sum == target:
      return min(nums[i..j-1]) + max(nums[i..j-1])

let encryptionWeakness = findEncryptionWeakness(input, invalidNumber)
echo encryptionWeakness
