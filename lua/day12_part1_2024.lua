
local function read_input(filename)
  local file = io.open(filename, "r")
  if not file then return nil end
  local lines = {}
  for line in file:lines() do
    table.insert(lines, line)
  end
  file:close()
  return lines
end

local function solve(lines)
  local rows = #lines
  local cols = #lines[1]
  local visited = {}
  local total_price = 0

  local function calculate_region(r, c, plant_type)
    if r < 1 or r > rows or c < 1 or c > cols or visited[r .. "," .. c] or lines[r]:sub(c, c) ~= plant_type then
      return 0, 0
    end

    visited[r .. "," .. c] = true
    local area = 1
    local perimeter = 0

    -- Check neighbors for perimeter
    local neighbors = {{r - 1, c}, {r + 1, c}, {r, c - 1}, {r, c + 1}}
    for _, neighbor in ipairs(neighbors) do
      local nr, nc = neighbor[1], neighbor[2]
      if nr < 1 or nr > rows or nc < 1 or nc > cols or lines[nr]:sub(nc, nc) ~= plant_type then
        perimeter = perimeter + 1
      end
    end

    -- Recursively explore neighbors
    for _, neighbor in ipairs(neighbors) do
      local nr, nc = neighbor[1], neighbor[2]
      local neighbor_area, neighbor_perimeter = calculate_region(nr, nc, plant_type)
      area = area + neighbor_area
      perimeter = perimeter + neighbor_perimeter
    end

    return area, perimeter
  end

  for r = 1, rows do
    for c = 1, cols do
      if not visited[r .. "," .. c] then
        local plant_type = lines[r]:sub(c, c)
        local area, perimeter = calculate_region(r, c, plant_type)
        if area > 0 then
          total_price = total_price + (area * perimeter)
        end
      end
    end
  end

  return total_price
end

local lines = read_input("input.txt")
if lines then
  local result = solve(lines)
  print(result)
end
