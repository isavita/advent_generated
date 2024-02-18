fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim()

houses = {}
location = {x: 0, y: 0}

for direction in input
  switch direction
    when '>'
      location.x++
    when '<'
      location.x--
    when '^'
      location.y++
    when 'v'
      location.y--

  houses["#{location.x},#{location.y}"] = true

console.log "Number of houses visited: #{Object.keys(houses).length}"
