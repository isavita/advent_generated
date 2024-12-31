
import strutils, sequtils, algorithm, os

proc concat(a, b: int): int =
  parseInt($a & $b)

proc canProduce(target: int, nums: seq[int], idx: int, value: int): bool =
  if idx == nums.len:
    return value == target
  let n = nums[idx]
  if canProduce(target, nums, idx + 1, value + n):
    return true
  if canProduce(target, nums, idx + 1, value * n):
    return true
  if canProduce(target, nums, idx + 1, concat(value, n)):
    return true
  return false

var total = 0
for line in lines("input.txt"):
  if line.len == 0:
    continue
  let parts = line.split(":")
  let target = parseInt(parts[0].strip())
  let numStrs = parts[1].strip().split()
  if numStrs.len == 0:
    continue
  let nums = numStrs.map(parseInt)

  if nums.len == 1:
    if nums[0] == target:
      total += target
    continue

  if canProduce(target, nums, 1, nums[0]):
    total += target

echo total
