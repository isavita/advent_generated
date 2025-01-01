
fs = require 'fs'

canProduce = (target, nums, idx, current) ->
  if idx is nums.length
    return current is target
  plus = canProduce target, nums, idx + 1, current + nums[idx]
  mul = canProduce target, nums, idx + 1, current * nums[idx]
  plus or mul

total = 0
lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')

for line in lines
  continue if not line
  [targetStr, numsStr] = line.split(':').map (s) -> s.trim()
  target = parseInt targetStr
  nums = numsStr.split(/\s+/).map (s) -> parseInt s
  if nums.length is 1
    if nums[0] is target
      total += target
    continue
  if canProduce target, nums, 1, nums[0]
    total += target

console.log total
