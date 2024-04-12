fs = require 'fs'

class Num
  constructor: (@pos, @val) ->

readAll = (path) ->
  fs.readFileSync(path, 'utf8').trim()

toInt = (s) ->
  parseInt(s, 10)

mix = (nums) ->
  n = nums.length - 1
  for num, i in nums
    oldpos = num.pos
    newpos = ((oldpos + num.val) % n + n) % n
    if oldpos < newpos
      for numj, j in nums
        if numj.pos > oldpos and numj.pos <= newpos
          numj.pos -= 1
    if newpos < oldpos
      for numj, j in nums
        if numj.pos >= newpos and numj.pos < oldpos
          numj.pos += 1
    num.pos = newpos

coords = (nums) ->
  l = nums.length
  zeroPos = 0
  for num, i in nums
    if num.val == 0
      zeroPos = num.pos
      break
  sum = 0
  for num, i in nums
    if num.pos == (zeroPos + 1000) % l or num.pos == (zeroPos + 2000) % l or num.pos == (zeroPos + 3000) % l
      sum += num.val
  sum

main = ->
  nums = []
  for n, i in readAll('input.txt').split '\n'
    nums.push new Num(i, toInt(n))
  nums2 = (new Num(num.pos, 811589153 * num.val) for num in nums)
  mix(nums)
  console.log coords(nums)

main()