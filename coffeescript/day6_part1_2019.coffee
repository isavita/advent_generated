
fs = require 'fs'

countOrbits = (orbitMap, start, depth) ->
  orbits = orbitMap[start]
  return depth if not orbits?
  depth + orbits.reduce((count, orbit) ->
    count + countOrbits(orbitMap, orbit, depth + 1)
  , 0)

readInput = (callback) ->
  fs.readFile 'input.txt', 'utf8', (err, data) ->
    throw err if err?
    callback data.trim().split('\n')

main = ->
  readInput (lines) ->
    orbitMap = {}
    for line in lines
      [center, orbiter] = line.split ')'
      (orbitMap[center] ||= []).push orbiter
    console.log countOrbits(orbitMap, 'COM', 0)

main()
