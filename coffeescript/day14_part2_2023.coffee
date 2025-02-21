
fs = require 'fs'

class Coord
  constructor: (@x, @y) ->

  add: (c2) -> new Coord(@x + c2.x, @y + c2.y)

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

    calculate_key: ()->
      key=0
      for own k,v of @data
        if v=='O'
          [x,y]=k.split(',').map (s)->parseInt(s)
          key+= x + y* @width
      key

    calculate_load: ()->
        load = 0
        for own k, char of @data
          if char == 'O'
            [x,y]=k.split(',').map (s)->parseInt(s)
            load += @height - y
        load
shift_single_rock = (grid, coord, direction) ->
    str_coord = "#{coord.x},#{coord.y}"
    if grid.data[str_coord] == 'O'
        current = coord
        before = coord.add(direction)
        while before.is_in_bounds(grid) and not grid.data["#{before.x},#{before.y}"]?
            grid.data["#{before.x},#{before.y}"] = 'O'
            delete grid.data[str_coord]
            current = before
            str_coord = "#{current.x},#{current.y}"
            before = before.add(direction)

shift_rocks = (grid, direction) ->
    if direction.x <0 or direction.y <0
      xs = [0...grid.width]
      ys = [0...grid.height]
    else
      xs = [grid.width-1..0]
      ys = [grid.height-1..0]

    for x in xs
        for y in ys
            shift_single_rock(grid, new Coord(x, y), direction)

cycle_rocks = (grid) ->
    shift_rocks(grid, new Coord(0, -1))
    shift_rocks(grid, new Coord(-1, 0))
    shift_rocks(grid, new Coord(0, 1))
    shift_rocks(grid, new Coord(1, 0))
solve = (input_data) ->
    num_cycles = 1000000000
    grid = new Grid(input_data)
    cache = {}

    i = 0
    while i < num_cycles
        grid_key = grid.calculate_key()
        if cache[grid_key]?
            i_start_cycle = cache[grid_key]
            cycle_length = i - i_start_cycle
            remaining_cycles = (num_cycles - i_start_cycle) % cycle_length
            for _ in [0...remaining_cycles]
                cycle_rocks(grid)
            return grid.calculate_load()
        cache[grid_key] = i
        cycle_rocks(grid)
        i++
    grid.calculate_load()

input_data = fs.readFileSync('input.txt', 'utf8').trim().split('\n')
console.log solve(input_data)
