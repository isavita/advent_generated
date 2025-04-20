
fs = require 'fs'

posToStr = (p) -> "#{p[0]},#{p[1]}"
stateToStr = (s) -> "#{s[0]},#{s[1]},#{s[2]}"

gcd = (a, b) ->
  a = Math.abs a
  b = Math.abs b
  while b
    [a, b] = [b, a % b]
  a

lcm = (a, b) ->
  if a is 0 or b is 0
    0
  else
    Math.abs(a * b) / gcd(a, b)

readInput = (filePath) ->
  walls = new Map()
  blizzards = []
  lines = fs.readFileSync(filePath, 'utf8').trim().split '\n'

  height = lines.length
  width = lines[0].length

  for y, line of lines
    for x, char of line
      if char is '#'
        walls.set posToStr([x, y]), true
      else if char in ['>', '<', '^', 'v']
        blizzards.push [x, y, char]

  [walls, blizzards, height, width]

findStartEnd = (walls, height, width) ->
  start = null
  end = null
  for x in [0...width]
    unless walls.has posToStr([x, 0])
      start = [x, 0]
      break
  for x in [0...width]
    unless walls.has posToStr([x, height - 1])
      end = [x, height - 1]
      break
  [start, end]

precomputeBlizzards = (blizzards, width, height, period) ->
  blizzardPositions = (new Map() for _ in [0...period])
  inner_width = width - 2
  inner_height = height - 2

  for t in [0...period]
    for blizzard in blizzards
      [x, y, dir] = blizzard
      
      if dir is '>'
        new_x = 1 + ((x - 1 + t) %% inner_width)
        new_y = y
      else if dir is '<'
        new_x = 1 + ((x - 1 - t) %% inner_width)
        new_y = y
      else if dir is 'v'
        new_x = x
        new_y = 1 + ((y - 1 + t) %% inner_height)
      else if dir is '^'
        new_x = x
        new_y = 1 + ((y - 1 - t) %% inner_height)

      blizzardPositions[t].set posToStr([new_x, new_y]), true

  blizzardPositions

bfs = (start, end, walls, blizzardPositions, period, width, height, startTime) ->
  queue = []
  visited = new Map()
  
  queue.push [start[0], start[1], startTime]
  visited.set stateToStr([start[0], start[1], startTime % period]), true

  directions = [[0, 0], [1, 0], [-1, 0], [0, 1], [0, -1]]

  while queue.length > 0
    [x, y, t] = queue.shift()

    if x is end[0] and y is end[1]
      return t

    next_t = t + 1
    blizzards_next = blizzardPositions[next_t % period]

    for [dx, dy] in directions
      nx = x + dx
      ny = y + dy
      
      if nx is end[0] and ny is end[1]
        return next_t

      state_key = stateToStr([nx, ny, next_t % period])

      is_start = nx is start[0] and ny is start[1]
      is_inner = nx > 0 and nx < width - 1 and ny > 0 and ny < height - 1

      if (is_start or is_inner)
        unless walls.has(posToStr([nx, ny])) or blizzards_next.has(posToStr([nx, ny])) or visited.has(state_key)
            visited.set state_key, true
            queue.push [nx, ny, next_t]

  -1

main = ->
  input_file = 'input.txt'
  [walls, blizzards, height, width] = readInput input_file
  [start, end] = findStartEnd walls, height, width
  
  inner_width = width - 2
  inner_height = height - 2
  period = lcm inner_width, inner_height
  
  blizzardPositions = precomputeBlizzards blizzards, width, height, period

  time1 = bfs start, end, walls, blizzardPositions, period, width, height, 0
  
  if time1 is -1
    console.error "No path found for trip 1."
    process.exit(1)

  time2 = bfs end, start, walls, blizzardPositions, period, width, height, time1

  if time2 is -1
    console.error "No path found for trip 2."
    process.exit(1)

  time3 = bfs start, end, walls, blizzardPositions, period, width, height, time2

  if time3 is -1
    console.error "No path found for trip 3."
    process.exit(1)

  console.log time3

main()
