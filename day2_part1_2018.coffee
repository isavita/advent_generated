
fs = require 'fs'

twoCount = threeCount = 0
input = fs.readFileSync('input.txt', 'utf8').split('\n')

for id in input
  charCount = {}
  for char in id
    charCount[char] ?= 0
    charCount[char]++
  hasTwos = false
  hasThrees = false
  for count in Object.values(charCount)
    if count == 2 then hasTwos = true
    if count == 3 then hasThrees = true
  if hasTwos then twoCount++
  if hasThrees then threeCount++

checksum = twoCount * threeCount
console.log checksum
