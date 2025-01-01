
fs = require 'fs'

solve = (grid) ->
  rows = grid.length
  return 0 if rows is 0
  cols = grid[0].length

  visited = (()->
    matrix = []
    for _ in [0...rows]
      matrix.push (()->
        row = []
        for _ in [0...cols]
          row.push false
        row
      )()
    matrix
  )()

  totalPrice = 0
  for r in [0...rows]
    for c in [0...cols]
      if not visited[r][c]
        {area, perimeter} = calculateRegion grid, r, c, visited
        totalPrice += area * perimeter
  totalPrice

calculateRegion = (grid, row, col, visited) ->
  rows = grid.length
  cols = grid[0].length
  char = grid[row][col]
  area = 0
  perimeter = 0

  queue = [{x: row, y: col}]
  visited[row][col] = true

  while queue.length > 0
    p = queue.shift()
    area++

    isBorder = p.x is 0 or p.x is rows - 1 or p.y is 0 or p.y is cols - 1

    checkNeighbor = (nx, ny) ->
      if nx >= 0 and nx < rows and ny >= 0 and ny < cols
        if grid[nx][ny] isnt char
          perimeter++
        else if not visited[nx][ny]
          queue.push {x: nx, y: ny}
          visited[nx][ny] = true
      else if isBorder
        perimeter++

    checkNeighbor p.x - 1, p.y
    checkNeighbor p.x + 1, p.y
    checkNeighbor p.x, p.y - 1
    checkNeighbor p.x, p.y + 1

  {area, perimeter}

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error "Error reading file:", err
    return
  grid = data.trim().split '\n'
  totalPrice = solve grid
  console.log totalPrice
