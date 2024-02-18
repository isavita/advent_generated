
x = [1]
fs = require('fs')
input = fs.readFileSync('input.txt').toString().split('\n')
for line in input
  switch line
    when 'noop' then x.push x[x.length - 1]
    else
      n = parseInt(line.split(' ')[1])
      x.push x[x.length - 1]
      x.push x[x.length - 1] + n
sum = 0
for i in [0...x.length]
  if (i - 19) % 40 is 0
    sum += (i + 1) * x[i]
console.log sum
