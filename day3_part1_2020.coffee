
fs = require('fs')

forest = []

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error err
    return

  rows = data.split('\n')
  for row in rows
    forest.push row

  trees = countTrees(forest, 3, 1)
  console.log trees

countTrees = (forest, right, down) ->
  trees = 0
  x = 0
  width = forest[0].length

  for y in [0...forest.length] by down
    if forest[y][x % width] == '#'
      trees++
    x += right

  return trees
