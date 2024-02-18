fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

total_ribbon = 0

for line in input
  dimensions = line.split('x').map ((dim) -> parseInt(dim))
  [l, w, h] = dimensions.sort((a, b) -> a - b)

  perimeter = 2 * (l + w) + l * w * h

  total_ribbon += perimeter

console.log "Total feet of ribbon needed: #{total_ribbon}"
