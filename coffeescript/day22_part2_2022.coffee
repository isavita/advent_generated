
fs = require 'fs'

class P
  constructor: (@x, @y) ->

class Dir
  @N: 0
  @E: 1
  @S: 2
  @W: 3
  
  constructor: (value) -> @value = value

  rotate: (direction) ->
    switch direction
      when 'R' then new Dir((@value + 1) % 4)
      when 'L' then new Dir((@value - 1 + 4) % 4)
      else @

  points: -> (@value + 3) % 4

class Movement
    constructor: (@steps = 0, @rotate = null) ->
    
class Human
    constructor: (@curr, @facing) ->

    walk: (map_data, dirs, size) ->
        dir_delta = dirs[@facing.value]
        next_pos  = new P(@curr.x + dir_delta.x, @curr.y + dir_delta.y)
        if next_pos.x of map_data and next_pos.y of map_data[next_pos.x]
            if map_data[next_pos.x][next_pos.y] then [@curr, @facing] else [next_pos, @facing]
        else
            [new_pos,new_facing] = cross_border(next_pos,@facing,size)
            if map_data[new_pos.x]? and map_data[new_pos.x][new_pos.y] then [@curr, @facing] else [new_pos,new_facing]

cross_border = (n, dir, size) ->
  {x, y} = n
  S = size

  if x == -1 and y < 2 * S then [new P(y + 2 * S, x + 1), new Dir(Dir.E)]
  else if x == -1 and y >= 2 * S then [new P(x + 4 * S, y - 2 * S), new Dir(Dir.N)]
  else if x == S and dir.value == Dir.S then [new P(y - S, x + S - 1), new Dir(Dir.W)]
  else if x == 2 * S - 1 and dir.value == Dir.N then [new P(y + S, x - S + 1), new Dir(Dir.E)]
  else if x == 3 * S and dir.value == Dir.S then [new P(y + 2 * S, x - 2 * S - 1), new Dir(Dir.W)]
  else if x == 4 * S then [new P(x - 4 * S, y + 2 * S), new Dir(Dir.S)]
  else if y == -1 and x < 3 * S then [new P(3 * S - 1 - x, y + S + 1), new Dir(Dir.E)]
  else if y == -1 and x >= 3 * S then [new P(y + 1, x - 2 * S), new Dir(Dir.S)]
  else if y == S - 1 and x < S then [new P(3 * S - 1 - x, y - S + 1), new Dir(Dir.E)]
  else if y == S - 1 and x >= S and dir.value == Dir.W then [new P(y + S + 1, x - S), new Dir(Dir.S)]
  else if y == S and dir.value == Dir.E then [new P(y + 2 * S - 1, x - 2 * S), new Dir(Dir.N)]
  else if y == 2 * S and x < 2 * S and dir.value == Dir.E then [new P(y - S - 1, x + S), new Dir(Dir.N)]
  else if y == 2 * S and x >= 2 * S then [new P(3 * S - 1 - x, y + S - 1), new Dir(Dir.W)]
  else if y == 3 * S then [new P(3 * S - 1 - x, y - S - 1), new Dir(Dir.W)]
  else throw new Error("Not a border crossing")

parse_path = (path) ->
  movements = []
  acc = 0
  for char in path
    if 'RL'.includes char
      movements.push(new Movement(acc)) if acc != 0
      acc = 0
      movements.push(new Movement(0, char))
    else if '0123456789'.includes char
      acc = acc * 10 + parseInt(char)
  movements.push(new Movement(acc)) if acc != 0
  movements

parse_input = (filename) ->
    lines = fs.readFileSync(filename, 'utf8').split('\n')
    map_data = {}
    size = 0
    r=0

    for line in lines
        line = line.trimEnd()
        break if line == ""
        size = line.length // 3 if r == 0

        for c,char of line
            continue if char == ' '
            map_data[r] ?= {}
            map_data[r][c] = char == '#'
        r++

    movement_line = lines[r+1].trim()
    movements = parse_path(movement_line)
    [map_data,size,movements]

main = ->
    [map_data, size, movements] = parse_input("input.txt")
    dirs = [
        new P(-1, 0)
        new P(0, 1)
        new P(1, 0)
        new P(0, -1)
    ]

    human = new Human(new P(0,size),new Dir(Dir.E))

    for mov in movements
        human.facing = human.facing.rotate(mov.rotate) if mov.rotate
        for i in [0...mov.steps]
            [new_pos,new_facing] = human.walk(map_data,dirs,size)
            if new_pos.x == human.curr.x and new_pos.y == human.curr.y and new_facing.value == human.facing.value
                break
            human.curr = new_pos
            human.facing = new_facing
    
    final_value = 1000 * (human.curr.x + 1) + 4 * (human.curr.y + 1) + human.facing.points()
    console.log final_value

main()
