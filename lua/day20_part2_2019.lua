
local io = require("io")
local math = require("math")
local table = require("table")
local string = require("string")

-- Optimized Queue Implementation
local Queue = {}
Queue.__index = Queue

function Queue.new()
  local self = setmetatable({}, Queue)
  self.head = 1
  self.tail = 1
  self.data = {}
  return self
end

function Queue:push(item)
  self.data[self.tail] = item
  self.tail = self.tail + 1
end

function Queue:pop()
  if self:is_empty() then
    return nil
  end
  local item = self.data[self.head]
  self.data[self.head] = nil -- Allow garbage collection
  self.head = self.head + 1
  return item
end

function Queue:is_empty()
  return self.head == self.tail
end

local function read_input(filename)
  local lines = {}
  local file = io.open(filename, "r")
  if not file then
    error("Could not open file: " .. filename)
  end
  for line in file:lines() do
    -- No rstrip needed as lines() usually handles newline trimming
    table.insert(lines, line)
  end
  file:close()
  return lines
end

local function is_letter(c)
    return c >= 'A' and c <= 'Z'
end

local function find_portals(grid)
  local portals = {}
  local height = #grid
  local width = 0
  for _, row in ipairs(grid) do
    width = math.max(width, #row)
  end

  local function add_portal(label, pos)
      if not portals[label] then
          portals[label] = {}
      end
      table.insert(portals[label], pos)
  end

  for y = 1, height do
    for x = 1, width do
      local char = string.sub(grid[y], x, x)
      if is_letter(char) then
        -- Check horizontal portal labels
        if x < width then
            local next_char = string.sub(grid[y], x+1, x+1)
            if is_letter(next_char) then
                local label = char .. next_char
                local pos = nil
                if x > 1 and string.sub(grid[y], x-1, x-1) == '.' then
                    pos = {x = x-1, y = y}
                elseif x + 2 <= width and string.sub(grid[y], x+2, x+2) == '.' then
                    pos = {x = x+2, y = y}
                end
                if pos then add_portal(label, pos) end
            end
        end
        -- Check vertical portal labels
        if y < height then
            local below_char = string.sub(grid[y+1] or "", x, x) -- Use "" for safety on last row
            if is_letter(below_char) then
                local label = char .. below_char
                local pos = nil
                if y > 1 and string.sub(grid[y-1], x, x) == '.' then
                    pos = {x = x, y = y-1}
                elseif y + 2 <= height and string.sub(grid[y+2], x, x) == '.' then
                     pos = {x = x, y = y+2}
                end
                 if pos then add_portal(label, pos) end
            end
        end
      end
    end
  end
  return portals, width, height
end

local function is_outer(x, y, width, height)
    -- Lua is 1-based, threshold adapted (<=3 includes 1,2,3)
    return x <= 3 or y <= 3 or x >= width - 2 or y >= height - 2
end

local function build_portal_mapping(portals, width, height)
    local portal_map = {}
    local start_pos = nil
    local end_pos = nil

    for label, positions in pairs(portals) do
        if label == "AA" then
            start_pos = positions[1]
        elseif label == "ZZ" then
            end_pos = positions[1]
        elseif #positions == 2 then
            local a = positions[1]
            local b = positions[2]
            local a_outer = is_outer(a.x, a.y, width, height)
            local b_outer = is_outer(b.x, b.y, width, height)

            local key_a = a.x .. "," .. a.y
            local key_b = b.x .. "," .. b.y

            portal_map[key_a] = { target = b, outer = a_outer }
            portal_map[key_b] = { target = a, outer = b_outer }
        end
    end
    return start_pos, end_pos, portal_map
end

local function bfs_recursive(grid, start_pos, end_pos, portal_map, width, height)
    local queue = Queue.new()
    queue:push({x = start_pos.x, y = start_pos.y, level = 0, steps = 0})

    local visited = {}
    local start_key = start_pos.x .. "," .. start_pos.y .. ",0"
    visited[start_key] = true

    local directions = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}}

    while not queue:is_empty() do
        local current = queue:pop()
        local x, y, level, steps = current.x, current.y, current.level, current.steps

        if x == end_pos.x and y == end_pos.y and level == 0 then
            return steps
        end

        -- Try moving in four directions
        for _, dir in ipairs(directions) do
            local dx, dy = dir[1], dir[2]
            local nx, ny = x + dx, y + dy

            if ny >= 1 and ny <= height and nx >= 1 and grid[ny] and nx <= #grid[ny] then -- Check bounds and row length
                 if string.sub(grid[ny], nx, nx) == '.' then
                    local state_key = nx .. "," .. ny .. "," .. level
                    if not visited[state_key] then
                        visited[state_key] = true
                        queue:push({x = nx, y = ny, level = level, steps = steps + 1})
                    end
                 end
            end
        end

        -- Check if on a portal tile
        local current_key = x .. "," .. y
        local portal_info = portal_map[current_key]

        if portal_info then
            local target = portal_info.target
            local outer = portal_info.outer
            local new_level = level + (outer and -1 or 1)

            if new_level >= 0 then
                local tx, ty = target.x, target.y
                local target_state_key = tx .. "," .. ty .. "," .. new_level
                if not visited[target_state_key] then
                    visited[target_state_key] = true
                    queue:push({x = tx, y = ty, level = new_level, steps = steps + 1})
                end
            end
        end
    end

    return -1 -- No solution found
end

local function main()
    local grid = read_input("input.txt")
    local portals, width, height = find_portals(grid)
    local start_node, end_node, portal_map = build_portal_mapping(portals, width, height)

    if not start_node or not end_node then
         print("Error: Start (AA) or End (ZZ) portal not found.")
         return
    end

    local result = bfs_recursive(grid, start_node, end_node, portal_map, width, height)
    print(result)
end

main()
