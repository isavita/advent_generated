
fs = require 'fs'

readInput = (filePath) ->
  fs.readFileSync(filePath, 'utf8').trim().split('\n').map (line) -> line.split ''

getEdgeStartPositions = (grid) ->
  height = grid.length
  width = grid[0].length
  starts = []
  for x in [0...width]
    starts.push [[x, 0], [0, 1]]
    starts.push [[x, height - 1], [0, -1]]
  for y in [1...height - 1]
    starts.push [[0, y], [1, 0]]
    starts.push [[width - 1, y], [-1, 0]]
  starts

reflect = (direction, mirror) ->
  [dx, dy] = direction
  if mirror is '/' then [-dy, -dx] else if mirror is '\\' then [dy, dx] else direction

splitBeam = (direction, splitter) ->
  [dx, dy] = direction
  if splitter is '|' and dx isnt 0 then [[0, -1], [0, 1]]
  else if splitter is '-' and dy isnt 0 then [[-1, 0], [1, 0]]
  else []

simulateBeam = (grid, startPos, startDir) ->
    height = grid.length
    width = grid[0].length
    queue = [[startPos[0], startPos[1], startDir]]
    visited = new Set()
    energized = new Set()

    while queue.length > 0
        [x, y, direction] = queue.shift()
        state = "#{x},#{y},#{direction[0]},#{direction[1]}"

        continue if visited.has state
        visited.add state
        energized.add "#{x},#{y}"

        [dx, dy] = direction
        [nx, ny] = [x + dx, y + dy]

        continue unless 0 <= nx < width and 0 <= ny < height

        cell = grid[ny][nx]

        if cell is '.'
            queue.push [nx, ny, direction]
        else if cell in ['/', '\\']
            newDir = reflect(direction, cell)
            queue.push [nx, ny, newDir]
        else if cell in ['|', '-']
            splitDirs = splitBeam(direction, cell)
            if splitDirs.length
                for newDir in splitDirs
                    queue.push [nx, ny, newDir]
            else
                queue.push [nx, ny, direction]
        else
            queue.push [nx, ny, direction]
    new Set(energized).size
    
partTwo = (grid) ->
  starts = getEdgeStartPositions grid
  starts.reduce ((maxEnergized, [startPos, startDir]) ->
    count = simulateBeam grid, startPos, startDir
    Math.max maxEnergized, count
  ), 0

main = ->
  grid = readInput 'input.txt'
  partOneStartPos = [0, 0]
  partOneStartDir = [1, 0]
  energizedPartOne = simulateBeam grid, partOneStartPos, partOneStartDir
  console.log "Part One Answer:", energizedPartOne
  maxEnergizedPartTwo = partTwo grid
  console.log "Part Two Answer:", maxEnergizedPartTwo

main()
