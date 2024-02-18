fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim().split('\n')

total_paper = 0

for line in input
  dimensions = line.split('x').map ((dim) -> parseInt(dim))
  [l, w, h] = dimensions.sort((a, b) -> a - b)

  surface_area = 2 * (l * w + w * h + h * l)
  slack = l * w

  total_paper += surface_area + slack

console.log "Total square feet of wrapping paper needed: #{total_paper}"
