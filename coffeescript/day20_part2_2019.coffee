
# Day 20: Donut Maze
# CoffeeScript solution for both parts

fs = require 'fs'

# Helper function to check if a character is an uppercase letter
is_letter = (char) -> char >= 'A' and char <= 'Z'

# Helper function to create a unique key for visited set
pos_key = (x, y, level = 0) -> "#{x},#{y},#{level}"

# Function to determine if a portal location is 'outer'
# Outer portals are near the edges (x=2, x=width-3, y=2, y=height-3)
is_outer_portal = (x, y, width, height) ->
  x == 2 or x == width - 3 or y == 2 or y == height - 3

# --- Main Solving Logic ---
solve = (grid, part2 = false) ->
  height = grid.length
  width = grid[0].length
  portals = {} # Stores label -> [{x, y}, {x, y}]
  portal_coords = {} # Stores "x,y" -> { label: string, is_outer: bool }
  start_pos = null
  end_pos = null

  # 1. Parse the grid to find passages, portals, start, and end
  for y in [0...height]
    for x in [0...width]
      char = grid[y][x]
      continue if not is_letter(char)

      # Check for vertical portal labels (letter above/below)
      if y > 0 and is_letter(grid[y-1][x])
        label = grid[y-1][x] + char
        # Check for passage below or above the label pair
        if y + 1 < height and grid[y+1][x] == '.' # Passage below
          p_x = x
          p_y = y + 1
        else if y - 2 >= 0 and grid[y-2][x] == '.' # Passage above
          p_x = x
          p_y = y - 2
        else
          continue # Should not happen with valid input

        portals[label] ?= []
        portals[label].push { x: p_x, y: p_y }
        outer = is_outer_portal(p_x, p_y, width, height)
        portal_coords[pos_key(p_x, p_y)] = { label: label, is_outer: outer }

      # Check for horizontal portal labels (letter left/right)
      else if x > 0 and is_letter(grid[y][x-1])
        label = grid[y][x-1] + char
        # Check for passage right or left of the label pair
        if x + 1 < width and grid[y][x+1] == '.' # Passage right
          p_x = x + 1
          p_y = y
        else if x - 2 >= 0 and grid[y][x-2] == '.' # Passage left
          p_x = x - 2
          p_y = y
        else
          continue # Should not happen with valid input

        portals[label] ?= []
        portals[label].push { x: p_x, y: p_y }
        outer = is_outer_portal(p_x, p_y, width, height)
        portal_coords[pos_key(p_x, p_y)] = { label: label, is_outer: outer }

  # Identify start (AA) and end (ZZ) positions
  start_pos = portals['AA'][0]
  end_pos = portals['ZZ'][0]

  # Build the warp map: "x1,y1" -> {x: x2, y: y2}
  warp_map = {}
  for label, coords of portals
    continue if coords.length isnt 2 # Skip AA and ZZ for warping
    [p1, p2] = coords
    key1 = pos_key(p1.x, p1.y)
    key2 = pos_key(p2.x, p2.y)
    warp_map[key1] = { x: p2.x, y: p2.y }
    warp_map[key2] = { x: p1.x, y: p1.y }

  # 2. Perform Breadth-First Search (BFS)
  queue = []
  visited = new Set()

  # Initial state
  start_state = { x: start_pos.x, y: start_pos.y, level: 0, steps: 0 }
  queue.push start_state
  visited.add pos_key(start_state.x, start_state.y, start_state.level)

  min_steps = -1 # Result if no path found

  while queue.length > 0
    current = queue.shift() # Dequeue
    { x, y, level, steps } = current

    # Check if we reached the target (ZZ at level 0)
    if x == end_pos.x and y == end_pos.y and level == 0
      min_steps = steps
      break

    # Explore neighbors (Up, Down, Left, Right)
    for [dx, dy] in [[0, -1], [0, 1], [-1, 0], [1, 0]]
      nx = x + dx
      ny = y + dy

      # Check bounds and if it's a passage
      if nx >= 0 and nx < width and ny >= 0 and ny < height and grid[ny][nx] == '.'
        next_key = pos_key(nx, ny, level)
        if not visited.has(next_key)
          visited.add next_key
          queue.push { x: nx, y: ny, level: level, steps: steps + 1 }

    # Check for portal warp
    current_key = pos_key(x, y)
    if portal_coords[current_key]? # Is the current position a portal entrance?
      portal_info = portal_coords[current_key]
      label = portal_info.label
      is_outer = portal_info.is_outer
      
      # Determine level change and apply constraints for Part 2
      next_level = level
      can_warp = true

      if part2
        if is_outer
          next_level = level - 1
        else # Inner portal
          next_level = level + 1

        # Constraints:
        if next_level < 0 # Cannot go above level 0
          can_warp = false
        else if level == 0 and not is_outer and (label == 'AA' or label == 'ZZ') # Inner AA/ZZ are walls from level 0 (shouldn't happen)
           can_warp = false # This case is technically impossible given start/end logic but included for completeness
        else if level == 0 and is_outer and label isnt 'AA' and label isnt 'ZZ' # Outer portals (not AA/ZZ) are walls at level 0
          can_warp = false
        else if level > 0 and (label == 'AA' or label == 'ZZ') # AA/ZZ are walls on inner levels
          can_warp = false

      # If warp is allowed and possible
      if can_warp and warp_map[current_key]?
        warp_dest = warp_map[current_key]
        warp_key = pos_key(warp_dest.x, warp_dest.y, next_level)

        if not visited.has(warp_key)
          visited.add warp_key
          queue.push { x: warp_dest.x, y: warp_dest.y, level: next_level, steps: steps + 1 }

  return min_steps


# --- Main Entry Point ---
main = ->
  try
    # Read input file
    input_data = fs.readFileSync('input.txt', 'utf8')
    grid = input_data.trimEnd().split('\n') # Use trimEnd to remove trailing newline only

    # --- Solve Part 1 ---
    # For Part 1, we treat it as a flat maze (no levels)
    # The 'solve' function handles this when part2 is false
    result_part1 = solve(grid, false)
    console.log "Part 1: Shortest path steps = #{result_part1}" # Expected: 714

    # --- Solve Part 2 ---
    # Enable recursive levels
    result_part2 = solve(grid, true)
    console.log "Part 2: Shortest path steps (recursive) = #{result_part2}"

  catch error
    console.error "Error reading file or processing maze:", error
    process.exit(1)

# Run the main function
main()
