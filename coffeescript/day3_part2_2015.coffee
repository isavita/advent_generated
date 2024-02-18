fs = require 'fs'

input = fs.readFileSync('input.txt', 'utf8').trim()

houses = {}
location = {x: 0, y: 0}
robo_location = {x: 0, y: 0}

deliver_present = (location) ->
  houses["#{location.x},#{location.y}"] = true

for i in [0...input.length] by 2
  direction = input[i]
  switch direction
    when '>'
      location.x++
    when '<'
      location.x--
    when '^'
      location.y++
    when 'v'
      location.y--
  deliver_present(location)

  direction = input[i+1]
  switch direction
    when '>'
      robo_location.x++
    when '<'
      robo_location.x--
    when '^'
      robo_location.y++
    when 'v'
      robo_location.y--
  deliver_present(robo_location)

console.log "Number of houses visited: #{Object.keys(houses).length}"
