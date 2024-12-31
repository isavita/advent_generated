
import strutils, sequtils, algorithm, os

proc canProduce(target: int, nums: seq[int], idx: int, current: int): bool =
  if idx == nums.len:
    return current == target
  let plus = canProduce(target, nums, idx + 1, current + nums[idx])
  let mul = canProduce(target, nums, idx + 1, current * nums[idx])
  return plus or mul

var total = 0
for line in lines("input.txt"):
  if line.len == 0:
    continue
  let parts = line.split(":")
  let target = parseInt(parts[0].strip())
  let numStrs = parts[1].strip().split()
  var nums = newSeq[int]()
  for ns in numStrs:
    nums.add(parseInt(ns))
  if nums.len == 1:
    if nums[0] == target:
      total += target
    continue
  if canProduce(target, nums, 1, nums[0]):
    total += target
echo total
