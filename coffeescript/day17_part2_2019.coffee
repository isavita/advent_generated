
#!/usr/bin/env coffee

fs = require 'fs'

class IntcodeComputer
  constructor: (memory_list) ->
    @memory = {}
    for val, i in memory_list
      @memory[i] = val
    @pointer = 0
    @relative_base = 0
    @inputs = []
    @outputs = []
    @halted = false

  get_mem: (addr) ->
    @memory[addr] ? 0

  set_mem: (addr, value) ->
    @memory[addr] = value

  get_param: (mode, param) ->
    switch mode
      when 0 then @get_mem(param) # Position mode
      when 1 then param           # Immediate mode
      when 2 then @get_mem(@relative_base + param) # Relative mode
      else throw new Error("Unknown parameter mode: #{mode}")

  set_param: (mode, param, value) ->
    switch mode
      when 0 then @set_mem(param, value) # Position mode
      when 2 then @set_mem(@relative_base + param, value) # Relative mode
      else throw new Error("Unknown parameter mode for writing: #{mode}")

  run: ->
    loop
      instruction = @get_mem(@pointer)
      opcode = instruction % 100
      modes = [
        Math.floor(instruction / 100) % 10,
        Math.floor(instruction / 1000) % 10,
        Math.floor(instruction / 10000) % 10
      ]

      p1 = @get_mem(@pointer + 1)
      p2 = @get_mem(@pointer + 2)
      p3 = @get_mem(@pointer + 3)

      switch opcode
        when 1 # Addition
          val1 = @get_param(modes[0], p1)
          val2 = @get_param(modes[1], p2)
          @set_param(modes[2], p3, val1 + val2)
          @pointer += 4
        when 2 # Multiplication
          val1 = @get_param(modes[0], p1)
          val2 = @get_param(modes[1], p2)
          @set_param(modes[2], p3, val1 * val2)
          @pointer += 4
        when 3 # Input
          unless @inputs.length > 0
            return # Wait for input
          input_value = @inputs.shift()
          @set_param(modes[0], p1, input_value)
          @pointer += 2
        when 4 # Output
          output_value = @get_param(modes[0], p1)
          @outputs.push(output_value)
          @pointer += 2
        when 5 # Jump-if-true
          val1 = @get_param(modes[0], p1)
          val2 = @get_param(modes[1], p2)
          if val1 != 0 then @pointer = val2 else @pointer += 3
        when 6 # Jump-if-false
          val1 = @get_param(modes[0], p1)
          val2 = @get_param(modes[1], p2)
          if val1 == 0 then @pointer = val2 else @pointer += 3
        when 7 # Less than
          val1 = @get_param(modes[0], p1)
          val2 = @get_param(modes[1], p2)
          @set_param(modes[2], p3, if val1 < val2 then 1 else 0)
          @pointer += 4
        when 8 # Equals
          val1 = @get_param(modes[0], p1)
          val2 = @get_param(modes[1], p2)
          @set_param(modes[2], p3, if val1 == val2 then 1 else 0)
          @pointer += 4
        when 9 # Adjust relative base
          val1 = @get_param(modes[0], p1)
          @relative_base += val1
          @pointer += 2
        when 99 # Halt
          @halted = true
          return
        else
          throw new Error("Unknown opcode: #{opcode} at pointer #{@pointer}")

read_input = (filename) ->
  content = fs.readFileSync(filename, 'utf8').trim()
  (Number(x) for x in content.split(','))

parse_map = (output) ->
  grid = []
  line = []
  for c in output
    if c == 10 # Newline
      if line.length > 0
        grid.push(line)
        line = []
    else
      line.push(String.fromCharCode(c))
  if line.length > 0 # Handle potential missing trailing newline
    grid.push(line)
  grid

find_intersections = (grid) ->
  intersections = []
  height = grid.length
  width = grid[0].length
  for y in [1...height - 1]
    for x in [1...width - 1]
      if grid[y][x] == '#' and
         grid[y - 1][x] == '#' and
         grid[y + 1][x] == '#' and
         grid[y][x - 1] == '#' and
         grid[y][x + 1] == '#'
        intersections.push([x, y])
  intersections

find_robot_position = (grid) ->
  for row, y in grid
    for cell, x in row
      if cell in ['^', 'v', '<', '>']
        return [x, y, cell]
  null

turn_left = (direction) ->
  switch direction
    when '^' then '<'
    when '<' then 'v'
    when 'v' then '>'
    when '>' then '^'

turn_right = (direction) ->
  switch direction
    when '^' then '>'
    when '>' then 'v'
    when 'v' then '<'
    when '<' then '^'

move_forward = (x, y, direction) ->
  switch direction
    when '^' then [x, y - 1]
    when 'v' then [x, y + 1]
    when '<' then [x - 1, y]
    when '>' then [x + 1, y]
    else throw new Error("Unknown direction: #{direction}")

get_movement_path = (grid, start_x, start_y, start_dir) ->
  [x, y, direction] = [start_x, start_y, start_dir]
  path = []
  steps = 0
  height = grid.length
  width = grid[0].length

  loop
    # Attempt to move forward
    [next_x, next_y] = move_forward(x, y, direction)
    can_move_forward = 0 <= next_y < height and 0 <= next_x < width and grid[next_y][next_x] == '#'

    if can_move_forward
      [x, y] = [next_x, next_y]
      steps += 1
    else
      # Record steps if any
      if steps > 0
        path.push(steps.toString())
        steps = 0

      # Try turning left
      left_dir = turn_left(direction)
      [lx, ly] = move_forward(x, y, left_dir)
      can_turn_left = 0 <= ly < height and 0 <= lx < width and grid[ly][lx] == '#'

      if can_turn_left
        path.push('L')
        direction = left_dir
        continue

      # Try turning right
      right_dir = turn_right(direction)
      [rx, ry] = move_forward(x, y, right_dir)
      can_turn_right = 0 <= ry < height and 0 <= rx < width and grid[ry][rx] == '#'

      if can_turn_right
        path.push('R')
        direction = right_dir
        continue

      # No further moves possible
      break
  path


arrays_equal = (arr1, arr2) ->
    return false if arr1.length != arr2.length
    for val, i in arr1
        return false if val != arr2[i]
    true

replace_sequence = (seq, pattern, replacement) ->
    res = []
    i = 0
    n = seq.length
    m = pattern.length
    while i < n
        if i + m <= n and arrays_equal(seq.slice(i, i + m), pattern)
            res.push(replacement)
            i += m
        else
            res.push(seq[i])
            i += 1
    res

compress_movement = (path) ->
  tokens = path # Already strings ['L', '10', 'R', '8', ...]
  max_function_length = 20
  max_pattern_length = 10 # Max tokens per function (turns + numbers)

  for a_len in [1..Math.min(max_pattern_length, tokens.length)]
    a_pattern = tokens.slice(0, a_len)
    a_str = a_pattern.join(',')
    continue if a_str.length > max_function_length

    tokens_after_a = replace_sequence(tokens, a_pattern, 'A')

    # Find first index not 'A'
    b_start_idx = -1
    for item, idx in tokens_after_a
      if item != 'A'
        b_start_idx = idx
        break
    continue if b_start_idx == -1

    for b_len in [1..Math.min(max_pattern_length, tokens.length - b_start_idx)]
      b_pattern = tokens_after_a.slice(b_start_idx, b_start_idx + b_len)
      # Ensure b_pattern doesn't contain 'A' itself (simplification, might need refinement)
      continue if b_pattern.includes('A')
      # Ensure b_pattern does not start with a sequence replaceable by A
      if a_len <= b_pattern.length and arrays_equal(b_pattern.slice(0, a_len), a_pattern)
         continue


      b_str = b_pattern.join(',')
      continue if b_str.length > max_function_length

      tokens_after_b = replace_sequence(tokens_after_a, b_pattern, 'B')

      # Find first index not 'A' or 'B'
      c_start_idx = -1
      for item, idx in tokens_after_b
        if item not in ['A', 'B']
          c_start_idx = idx
          break
      continue if c_start_idx == -1


      for c_len in [1..Math.min(max_pattern_length, tokens.length - c_start_idx)]
        c_pattern = tokens_after_b.slice(c_start_idx, c_start_idx + c_len)
        # Ensure c_pattern doesn't contain 'A' or 'B'
        continue if c_pattern.includes('A') or c_pattern.includes('B')
        # Ensure c_pattern doesn't start with sequence replaceable by A or B
        if a_len <= c_pattern.length and arrays_equal(c_pattern.slice(0, a_len), a_pattern)
           continue
        if b_len <= c_pattern.length and arrays_equal(c_pattern.slice(0, b_len), b_pattern)
           continue


        c_str = c_pattern.join(',')
        continue if c_str.length > max_function_length

        tokens_after_c = replace_sequence(tokens_after_b, c_pattern, 'C')

        # Check if only A, B, C remain
        if tokens_after_c.every((x) => x in ['A', 'B', 'C'])
            main_routine = tokens_after_c.join(',')
            if main_routine.length <= 20
                return [main_routine, a_str, b_str, c_str]

  throw new Error("Could not compress the path into functions A, B, C.")

main = ->
  program = read_input('input.txt')

  # ----- Part One -----
  computer = new IntcodeComputer(program.slice()) # Use slice for copy
  computer.run()
  output = computer.outputs.slice() # Copy outputs
  grid = parse_map(output)

  # Optional: Print map for debugging
  # for row in grid
  #   console.log row.join('')

  intersections = find_intersections(grid)
  alignment_sum = 0
  for [x, y] in intersections
    alignment_sum += x * y
  console.log "Part One: Sum of alignment parameters = #{alignment_sum}"

  # ----- Part Two -----
  program_part2 = program.slice()
  program_part2[0] = 2
  computer_part2 = new IntcodeComputer(program_part2)

  robot = find_robot_position(grid)
  unless robot
    throw new Error("Robot not found on the scaffold.")
  [start_x, start_y, start_dir] = robot

  movement_path = get_movement_path(grid, start_x, start_y, start_dir)
  # console.log "Movement Path:", movement_path.join(',') # Debugging

  [main_routine, function_A, function_B, function_C] = [null, null, null, null]
  try
    [main_routine, function_A, function_B, function_C] = compress_movement(movement_path)
  catch error
    console.error "Error in compressing path:", error
    process.exit(1)

  # console.log "Main:", main_routine # Debugging
  # console.log "A:", function_A
  # console.log "B:", function_B
  # console.log "C:", function_C

  input_lines = [
    main_routine
    function_A
    function_B
    function_C
    'n'
  ]
  movement_inputs = []
  for line in input_lines
    for char in line
      movement_inputs.push(char.charCodeAt(0))
    movement_inputs.push(10) # Newline

  computer_part2.inputs = movement_inputs

  while not computer_part2.halted
    computer_part2.run()

  # The dust collected is the last output value
  dust_collected = computer_part2.outputs[computer_part2.outputs.length - 1]

  console.log "Part Two: Dust collected = #{dust_collected}"


main()
