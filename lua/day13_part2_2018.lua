
local function parse_input(file_path)
  local tracks = {}
  local carts = {}
  local y = 0
  local file = io.open(file_path, "r")
  if not file then
    error("Could not open file: " .. file_path)
  end

  for line in file:lines() do
    for x = 0, #line - 1 do
      local char = string.sub(line, x + 1, x + 1)
      local key = x .. "," .. y
      if char == '^' or char == 'v' or char == '<' or char == '>' then
        local cart = {
          position = {x, y},
          direction = char,
          next_turn = "left" -- 0: left, 1: straight, 2: right
        }
        table.insert(carts, cart)
        if char == '^' or char == 'v' then
          tracks[key] = '|'
        else
          tracks[key] = '-'
        end
      elseif char == '|' or char == '-' or char == '+' or char == '/' or char == '\\' then
        tracks[key] = char
      end
    end
    y = y + 1
  end
  file:close()
  return tracks, carts
end

local turn_slash = { ['^'] = '>', ['v'] = '<', ['<'] = 'v', ['>'] = '^' }
local turn_backslash = { ['^'] = '<', ['v'] = '>', ['<'] = '^', ['>'] = 'v' }
local turn_left = { ['^'] = '<', ['v'] = '>', ['<'] = 'v', ['>'] = '^' }
local turn_right = { ['^'] = '>', ['v'] = '<', ['<'] = '^', ['>'] = 'v' }
local next_turn_map = { left = "straight", straight = "right", right = "left" }
local move_delta = {
  ['^'] = {0, -1}, ['v'] = {0, 1}, ['<'] = {-1, 0}, ['>'] = {1, 0}
}

local function move_cart(cart, tracks)
  local x, y = cart.position[1], cart.position[2]
  local direction = cart.direction

  local delta = move_delta[direction]
  local dx, dy = delta[1], delta[2]

  local new_x, new_y = x + dx, y + dy
  local new_pos_key = new_x .. "," .. new_y
  local track = tracks[new_pos_key]
  local new_dir = direction

  if track == '/' then
    new_dir = turn_slash[direction]
  elseif track == '\\' then
    new_dir = turn_backslash[direction]
  elseif track == '+' then
    if cart.next_turn == "left" then
      new_dir = turn_left[direction]
    elseif cart.next_turn == "right" then
      new_dir = turn_right[direction]
    end
    cart.next_turn = next_turn_map[cart.next_turn]
  end

  cart.position = {new_x, new_y}
  cart.direction = new_dir
end

local function simulate(tracks, carts)
  local positions = {}

  while #carts > 1 do
    table.sort(carts, function(a, b)
      if a.position[2] < b.position[2] then return true end
      if a.position[2] > b.position[2] then return false end
      return a.position[1] < b.position[1]
    end)

    local to_remove = {}
    positions = {}
    for i = 1, #carts do
        local cart = carts[i]
        local key = cart.position[1] .. "," .. cart.position[2]
        positions[key] = i
    end

    local next_carts = {}
    local removed_indices = {}

    for i = 1, #carts do
      if not removed_indices[i] then
        local cart = carts[i]
        local old_key = cart.position[1] .. "," .. cart.position[2]
        positions[old_key] = nil -- Remove old position marker

        move_cart(cart, tracks)

        local new_key = cart.position[1] .. "," .. cart.position[2]

        if positions[new_key] then
           local other_cart_index = positions[new_key]
           removed_indices[i] = true
           removed_indices[other_cart_index] = true
           positions[new_key] = nil -- Remove collided position marker
        else
            positions[new_key] = i -- Mark new position
        end
      end
    end

    local current_carts = {}
    for i = 1, #carts do
        if not removed_indices[i] then
            table.insert(current_carts, carts[i])
        end
    end
    carts = current_carts

  end

  if #carts == 1 then
    return carts[1].position
  else
    -- Should not happen based on problem description, but handle gracefully
    return {-1, -1}
  end
end

local function main()
  local tracks, carts = parse_input("input.txt")
  local last_pos = simulate(tracks, carts)
  print(string.format("%d,%d", last_pos[1], last_pos[2]))
end

main()
