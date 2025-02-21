
fs = require 'fs'

solve = (grid, startRow, startCol, startDir) ->
  rows = grid.length
  cols = grid[0].length

  energized = Array(rows).fill(null).map -> Array(cols).fill(false)
  visited = Array(rows).fill(null).map -> Array(cols).fill({}).map -> { up: false, down: false, left: false, right: false }
  queue = [[startRow, startCol, startDir]]

  while queue.length > 0
    [row, col, dir] = queue.shift()

    continue unless row >= 0 and row < rows and col >= 0 and col < cols
    continue if visited[row][col][dir]

    energized[row][col] = true
    visited[row][col][dir] = true

    tile = grid[row][col]

    switch tile
      when '.'
        switch dir
          when 'right'
            queue.push([row, col + 1, 'right'])
          when 'left'
            queue.push([row, col - 1, 'left'])
          when 'up'
            queue.push([row - 1, col, 'up'])
          when 'down'
            queue.push([row + 1, col, 'down'])
      when '/'
        switch dir
          when 'right'
            queue.push([row - 1, col, 'up'])
          when 'left'
            queue.push([row + 1, col, 'down'])
          when 'up'
            queue.push([row, col + 1, 'right'])
          when 'down'
            queue.push([row, col - 1, 'left'])
      when '\\'
        switch dir
          when 'right'
            queue.push([row + 1, col, 'down'])
          when 'left'
            queue.push([row - 1, col, 'up'])
          when 'up'
            queue.push([row, col - 1, 'left'])
          when 'down'
            queue.push([row, col + 1, 'right'])
      when '|'
        switch dir
          when 'right', 'left'
            queue.push([row - 1, col, 'up'])
            queue.push([row + 1, col, 'down'])
          when 'up'
            queue.push([row - 1, col, 'up'])
          when 'down'
            queue.push([row + 1, col, 'down'])
      when '-'
        switch dir
          when 'up', 'down'
            queue.push([row, col - 1, 'left'])
            queue.push([row, col + 1, 'right'])
          when 'right'
            queue.push([row, col + 1, 'right'])
          when 'left'
            queue.push([row, col - 1, 'left'])

  energized.reduce ((count, row) -> count + row.filter((cell) -> cell).length), 0

fs.readFile 'input.txt', 'utf8', (err, data) ->
  if err
    console.error err
    return

  grid = data.trim().split('\n').map (line) -> line.split('')
  energizedCount = solve(grid, 0, 0, 'right')
  console.log energizedCount
