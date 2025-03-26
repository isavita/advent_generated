
local function read_maze(filename)
  local maze = {}
  local file = io.open(filename, 'r')
  if not file then
    error("Could not open file: " .. filename)
  end
  for line in file:lines() do
    local row = {}
    for i = 1, #line do
      table.insert(row, line:sub(i, i))
    end
    table.insert(maze, row)
  end
  file:close()
  return maze
end

local function is_upper(char)
    if not char then return false end
    local byte = string.byte(char)
    return byte >= string.byte('A') and byte <= string.byte('Z')
end

local function find_portals(maze)
  local height = #maze
  local width = #maze[1]
  local portals = {}
  local portal_positions = {}

  for y = 1, height do
    for x = 1, width do
      local char = maze[y][x]
      if is_upper(char) then
        -- Check right
        if x + 1 <= width and is_upper(maze[y][x+1]) then
          local portal_name = char .. maze[y][x+1]
          local px, py
          if x + 2 <= width and maze[y][x+2] == '.' then
            px, py = x + 2, y
          elseif x - 1 >= 1 and maze[y][x-1] == '.' then
            px, py = x - 1, y
          end
          if px then
            portals[portal_name] = portals[portal_name] or {}
            table.insert(portals[portal_name], {px, py})
            portal_positions[px .. "," .. py] = portal_name
          end
        end
        -- Check down
        if y + 1 <= height and is_upper(maze[y+1][x]) then
          local portal_name = char .. maze[y+1][x]
          local px, py
          if y + 2 <= height and maze[y+2][x] == '.' then
            px, py = x, y + 2
          elseif y - 1 >= 1 and maze[y-1][x] == '.' then
            px, py = x, y - 1
          end
          if px then
            portals[portal_name] = portals[portal_name] or {}
            table.insert(portals[portal_name], {px, py})
            portal_positions[px .. "," .. py] = portal_name
          end
        end
      end
    end
  end
  return portals, portal_positions
end

local function bfs(maze, portals, portal_positions, start_coord, end_coord)
  local queue = {}
  local visited = {}
  local width = #maze[1]
  local height = #maze

  local start_x, start_y = start_coord[1], start_coord[2]
  local end_x, end_y = end_coord[1], end_coord[2]

  table.insert(queue, {start_x, start_y, 0}) -- {x, y, steps}
  visited[start_x .. "," .. start_y] = true

  local head = 1

  while head <= #queue do
    local current = queue[head]
    head = head + 1
    local x, y, steps = current[1], current[2], current[3]

    local deltas = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}
    for _, delta in ipairs(deltas) do
      local dx, dy = delta[1], delta[2]
      local nx, ny = x + dx, y + dy

      if nx >= 1 and nx <= width and ny >= 1 and ny <= height and maze[ny][nx] == '.' then
        if nx == end_x and ny == end_y then
          return steps + 1
        end
        local key = nx .. "," .. ny
        if not visited[key] then
          visited[key] = true
          table.insert(queue, {nx, ny, steps + 1})
        end
      end
    end

    local current_key = x .. "," .. y
    local portal_name = portal_positions[current_key]
    if portal_name then
        local destinations = portals[portal_name]
        if destinations then
            for _, dest_coord in ipairs(destinations) do
                local px, py = dest_coord[1], dest_coord[2]
                if not (px == x and py == y) then -- Don't jump back to the same spot
                    local key = px .. "," .. py
                     if not visited[key] then
                        visited[key] = true
                        table.insert(queue, {px, py, steps + 1}) -- Portal jump costs 1 step
                    end
                end
            end
        end
    end
  end

  return -1 -- Not found
end

local function main()
  local maze = read_maze("input.txt")
  local portals, portal_positions = find_portals(maze)

  if not portals['AA'] or not portals['ZZ'] then
      error("Start (AA) or End (ZZ) portal not found.")
  end

  local start_coord = portals['AA'][1]
  local end_coord = portals['ZZ'][1]

  local result = bfs(maze, portals, portal_positions, start_coord, end_coord)
  print(result)
end

main()
