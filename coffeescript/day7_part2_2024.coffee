
fs = require 'fs'

concat = (a, b) -> parseInt("" + a + b)

canProduce = (target, nums, idx, value, cache = {}) ->
  key = "#{idx},#{value}"
  return cache[key] if cache[key]?
  if idx is nums.length
    return cache[key] = value is target
  n = nums[idx]
  if canProduce(target, nums, idx + 1, value + n, cache)
    return cache[key] = true
  if canProduce(target, nums, idx + 1, value * n, cache)
    return cache[key] = true
  if canProduce(target, nums, idx + 1, concat(value, n), cache)
    return cache[key] = true
  return cache[key] = false

total = 0
lines = fs.readFileSync('input.txt', 'utf-8').trim().split('\n')
for line in lines
  continue if not line
  [targetStr, numsStr] = line.split(':').map((s) -> s.trim())
  target = parseInt(targetStr)
  nums = numsStr.split(/\s+/).map((s) -> parseInt(s))
  continue if nums.length is 0
  if nums.length is 1
    if nums[0] is target
      total += target
    continue
  if canProduce(target, nums, 1, nums[0])
    total += target

console.log total
