
fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8')
re = /(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))/g
enabled = true
totalSum = 0
match
while match = re.exec(input)
  if match[1]
    if enabled
      totalSum += parseInt(match[2]) * parseInt(match[3])
  else if match[4]
    enabled = true
  else if match[5]
    enabled = false
console.log totalSum
