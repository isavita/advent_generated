
local function read_file(filename)
  local file = io.open(filename, "r")
  if not file then return nil end
  local content = file:read("*all")
  file:close()
  return content
end

local function solve(input)
  local grid = {}
  local start_x, start_y, direction
  local lines = {}
  for line in input:gmatch("([^\n]+)") do
    table.insert(lines, line)
  end
  
  for y, line in ipairs(lines) do
    grid[y] = {}
    for x = 1, #line do
      grid[y][x] = line:sub(x, x)
      if grid[y][x] == '^' then
        start_x, start_y = x, y
        direction = 1 -- 1: up, 2: right, 3: down, 4: left
      end
    end
  end

  local visited = {}
  local x, y = start_x, start_y
  local count = 0
  
  local function is_valid(x, y)
    return grid[y] and grid[y][x]
  end

  local function is_obstacle(x, y)
    return is_valid(x, y) and grid[y][x] == '#'
  end

  local function mark_visited(x, y)
    local key = x .. "," .. y
    if not visited[key] then
      visited[key] = true
      count = count + 1
    end
  end

  while true do
    mark_visited(x, y)
    local next_x, next_y
    if direction == 1 then
      next_x, next_y = x, y - 1
    elseif direction == 2 then
      next_x, next_y = x + 1, y
    elseif direction == 3 then
      next_x, next_y = x, y + 1
    else
      next_x, next_y = x - 1, y
    end

    if is_obstacle(next_x, next_y) then
      direction = (direction % 4) + 1
    else
      x, y = next_x, next_y
    end
    
    if not is_valid(x,y) then
        break
    end
  end

  return count
end

local input = read_file("input.txt")
if input then
  local result = solve(input)
  print(result)
end
