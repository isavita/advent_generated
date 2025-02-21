
fs = require 'fs'

class Coord
  constructor: (@x, @y) ->

  add: (other) -> new Coord(@x + other.x, @y + other.y)

  is_in_bounds: (grid) -> 0 <= @x < grid.width and 0 <= @y < grid.height

class Grid
    constructor: (input_data) ->
        @width = input_data[0].length
        @height = input_data.length
        @data = {}
        for y in [0...@height]
            for x in [0...@width]
                if input_data[y][x] != '.'
                    @data["#{x},#{y}"] = input_data[y][x]

shift_single_rock = (grid, coord, direction) ->
    key = "#{coord.x},#{coord.y}"
    if grid.data[key] == 'O'
        current = coord
        before = coord.add(direction)
        while before.is_in_bounds(grid) and not grid.data["#{before.x},#{before.y}"]?
            grid.data["#{before.x},#{before.y}"] = 'O'
            delete grid.data[key]
            current = before
            key = "#{current.x},#{current.y}"
            before = before.add(direction)
    
shift_rocks = (grid, direction) ->
    if direction.y == -1 or direction.x == -1
      for x in [0...grid.width]
          for y in [0...grid.height]
              shift_single_rock(grid, new Coord(x, y), direction)
    else
      for x in [grid.width - 1..0] by -1
          for y in [grid.height - 1..0] by -1
              shift_single_rock(grid, new Coord(x,y), direction)

calculate_load = (grid) ->
  load = 0
  for own key, value of grid.data
      if value == 'O'
          [x, y] = key.split(',').map (str) -> parseInt(str)
          load += grid.height - y
  load

solve = (input_data) ->
  grid = new Grid(input_data)
  shift_rocks(grid, new Coord(0, -1))
  calculate_load grid

input_data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
console.log solve(input_data)
