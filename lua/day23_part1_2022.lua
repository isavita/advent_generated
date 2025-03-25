
local function read_input(file_path)
  local elves = {}
  local file = io.open(file_path, "r")
  if not file then return nil end

  local y = 0
  for line in file:lines() do
    y = y + 1
    for x = 1, #line do
      if string.sub(line, x, x) == "#" then
        elves[#elves + 1] = {x = x, y = y}
      end
    end
  end
  file:close()
  return elves
end

local function get_adjacent_positions(x, y)
  return {
    {x = x - 1, y = y - 1}, {x = x, y = y - 1}, {x = x + 1, y = y - 1},
    {x = x - 1, y = y},           {x = x + 1, y = y},
    {x = x - 1, y = y + 1}, {x = x, y = y + 1}, {x = x + 1, y = y + 1}
  }
end

local function simulate_rounds(elves, rounds)
  local directions = {
    {move_x = 0, move_y = -1, checks = { {0, -1}, {1, -1}, {-1, -1} }}, -- N
    {move_x = 0, move_y = 1, checks = { {0, 1}, {1, 1}, {-1, 1} }},  -- S
    {move_x = -1, move_y = 0, checks = { {-1, 0}, {-1, -1}, {-1, 1} }}, -- W
    {move_x = 1, move_y = 0, checks = { {1, 0}, {1, -1}, {1, 1} }}   -- E
  }

  for round_num = 1, rounds do
    local proposals = {}
    local proposed_moves = {}
    local elf_positions = {}
    for i, elf in ipairs(elves) do
      elf_positions[elf.x .. "," .. elf.y] = true
    end

    for i, elf in ipairs(elves) do
      local x, y = elf.x, elf.y
      local adjacent = get_adjacent_positions(x, y)
      local has_neighbor = false
      for _, pos in ipairs(adjacent) do
        if elf_positions[pos.x .. "," .. pos.y] then
          has_neighbor = true
          break
        end
      end
      if not has_neighbor then
        goto continue_elf
      end

      for dir_index = 1, #directions do
        local direction = directions[dir_index]
        local can_move = true
        for _, check in ipairs(direction.checks) do
          local check_x, check_y = x + check[1], y + check[2]
          if elf_positions[check_x .. "," .. check_y] then
            can_move = false
            break
          end
        end
        if can_move then
          local dest_x, dest_y = x + direction.move_x, y + direction.move_y
          local dest_key = dest_x .. "," .. dest_y
          if not proposals[dest_key] then
            proposals[dest_key] = {}
          end
          table.insert(proposals[dest_key], i)
          proposed_moves[i] = {x = dest_x, y = dest_y}
          break
        end
      end

      ::continue_elf::
    end

    local new_elves = {}
    for i, elf in ipairs(elves) do
      local move = proposed_moves[i]
      if move then
        local dest_key = move.x .. "," .. move.y
        if #proposals[dest_key] == 1 then
          table.insert(new_elves, move)
        else
          table.insert(new_elves, elf)
        end
      else
        table.insert(new_elves, elf)
      end
    end
    elves = new_elves
    table.insert(directions, table.remove(directions, 1))
  end

  return elves
end

local function calculate_empty_ground(elves)
  local min_x, max_x = elves[1].x, elves[1].x
  local min_y, max_y = elves[1].y, elves[1].y

  for _, elf in ipairs(elves) do
    min_x = math.min(min_x, elf.x)
    max_x = math.max(max_x, elf.x)
    min_y = math.min(min_y, elf.y)
    max_y = math.max(max_y, elf.y)
  end

  local width = max_x - min_x + 1
  local height = max_y - min_y + 1
  local total_tiles = width * height
  local empty_tiles = total_tiles - #elves
  return empty_tiles
end

local function main()
  local input_file = "input.txt"
  local elves = read_input(input_file)
  elves = simulate_rounds(elves, 10)
  local empty_ground = calculate_empty_ground(elves)
  print(empty_ground)
end

main()
