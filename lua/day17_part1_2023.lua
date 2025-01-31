
local function build_grid(input)
  local width = #input[1]
  local height = #input
  local data = {}
  for y, line in ipairs(input) do
    for x = 1, width do
      local coord = { x = x - 1, y = y - 1 }
      data[coord.y * width + coord.x] = tonumber(line:sub(x, x))
    end
  end
  return { width = width, height = height, data = data }
end

local function neighbors4(grid, coord)
  local neighbors = {}
  local directions = {
    { x = 0, y = -1 },
    { x = -1, y = 0 },
    { x = 0, y = 1 },
    { x = 1, y = 0 }
  }
  for _, dir in ipairs(directions) do
    local neighbor = { x = coord.x + dir.x, y = coord.y + dir.y }
    if 0 <= neighbor.x and neighbor.x < grid.width and 0 <= neighbor.y and neighbor.y < grid.height then
      table.insert(neighbors, neighbor)
    end
  end
  return neighbors
end

local function heuristic(c1, c2)
  return math.abs(c1.x - c2.x) + math.abs(c1.y - c2.y)
end

local function solve(input)
  local grid = build_grid(input)
  local start = { x = 0, y = 0 }
  local goal = { x = grid.width - 1, y = grid.height - 1 }

  local function a_star_constrained(start, goal, min_straight, max_straight)
    local function hash_info(info)
      return info.coord.y * grid.width * grid.width * grid.height * 11 +
        info.coord.x * grid.width * grid.height * 11 +
        (info.dir.y + 1) * grid.width * grid.height * 4 +
        (info.dir.x + 1) * grid.width * grid.height + info.num_straight
    end
    local start_info = { coord = start, dir = { x = 0, y = 0 }, num_straight = 0 }

    local frontier = {}
    local came_from = {}
    local cost_so_far = {}

    table.insert(frontier, { item = start_info, priority = 0 })
    came_from[hash_info(start_info)] = start_info
    cost_so_far[hash_info(start_info)] = 0

    local function reconstruct_path(current)
        local total_path = {current}
        while came_from[hash_info(current)] do
          current = came_from[hash_info(current)]
          table.insert(total_path, 1, current)
        end
        return total_path
      end

    while #frontier > 0 do
      table.sort(frontier, function(a, b) return a.priority < b.priority end)
      local current = table.remove(frontier, 1).item
      local current_cost = cost_so_far[hash_info(current)]

      if current.coord.x == goal.x and current.coord.y == goal.y then
        return current_cost
      end

      for _, next in ipairs(neighbors4(grid, current.coord)) do
        local new_dir = { x = next.x - current.coord.x, y = next.y - current.coord.y }
        local new_num_straight = 1
        if new_dir.x == current.dir.x and new_dir.y == current.dir.y then
          new_num_straight = new_num_straight + current.num_straight
        end

        local next_info = { coord = next, dir = new_dir, num_straight = new_num_straight }
        local new_cost = current_cost + grid.data[next.y * grid.width + next.x]
        local actual_cost = cost_so_far[hash_info(next_info)]

        local is_lower_cost = actual_cost == nil or new_cost < actual_cost
        local is_valid_straight =
          (current.num_straight >= min_straight or (new_dir.x == current.dir.x and new_dir.y == current.dir.y) or
            (current.coord.x == start.x and current.coord.y == start.y)) and
          new_num_straight <= max_straight
        local is_not_opposite_direction = new_dir.x ~= -current.dir.x or new_dir.y ~= -current.dir.y

        local is_valid = is_lower_cost and is_valid_straight and is_not_opposite_direction
        if is_valid then
          cost_so_far[hash_info(next_info)] = new_cost
          came_from[hash_info(next_info)] = current
          local priority = new_cost + heuristic(next, goal)
          table.insert(frontier, { item = next_info, priority = priority })
        end
      end
    end
    return -1
  end

  return a_star_constrained(start, goal, 0, 3)
end

local file = io.open("input.txt", "r")
local input = {}
for line in file:lines() do
  table.insert(input, line)
end
file:close()
print(solve(input))
