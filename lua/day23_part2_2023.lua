
local function coord_to_string(c)
  return string.format("%d,%d", c.x, c.y)
end

local function string_to_coord(s)
    local x, y = s:match("([^,]+),([^,]+)")
    return { x = tonumber(x), y = tonumber(y) }
end


local Coord = {}
Coord.__index = Coord

function Coord:new(x, y)
  local self = setmetatable({}, Coord)
  self.x = x
  self.y = y
  return self
end

function Coord.__add(a, b)
  return Coord:new(a.x + b.x, a.y + b.y)
end

function Coord.__eq(a, b)
    if not a or not b then return false end
    return a.x == b.x and a.y == b.y
end

function Coord:tostring()
    return coord_to_string(self)
end

local North = Coord:new(0, -1)
local South = Coord:new(0, 1)
local West = Coord:new(-1, 0)
local East = Coord:new(1, 0)

local Empty = '.'
local Wall = '#'
-- Note: Slopes are ignored in part 2 logic as per the Python solution's use of is_valid_neighbor
-- local NorthSlopes = '^'
-- local SouthSlopes = 'v'
-- local WestSlopes = '<'
-- local EastSlopes = '>'

-- local SlopeToDir = {
--     [NorthSlopes] = North,
--     [SouthSlopes] = South,
--     [WestSlopes] = West,
--     [EastSlopes] = East,
-- }


local Grid = {}
Grid.__index = Grid

function Grid:new(width, height, data)
  local self = setmetatable({}, Grid)
  self.width = width
  self.height = height
  self.data = data or {}
  return self
end

local Graph = {}
Graph.__index = Graph

function Graph:new()
    local self = setmetatable({}, Graph)
    self.vertices = {} -- Use table as set: keys are coord strings, values are true
    self.edges = {}    -- Adjacency list: keys are coord strings, values are tables {neighbor_coord_str = weight}
    return self
end


local function is_in_bounds(grid, coord)
  return coord.x >= 0 and coord.x < grid.width and coord.y >= 0 and coord.y < grid.height
end

local function parse_input(lines)
  local height = #lines
  local width = #lines[1]
  local data = {}
  for y, line in ipairs(lines) do
    for x = 1, width do
      local char = line:sub(x, x)
      if char ~= Empty then
        data[coord_to_string(Coord:new(x - 1, y - 1))] = char
      end
    end
  end
  return Grid:new(width, height, data)
end

-- is_valid_neighbor function matching the Python usage (ignores slopes for part 2)
local function is_valid_neighbor(grid, coord, dir)
  if not is_in_bounds(grid, coord) then
    return false
  end
  if grid.data[coord_to_string(coord)] == Wall then
    return false
  end
  return true
end


local function neighbors4(grid, coord, is_valid_neighbor_func)
  local directions = {North, South, West, East}
  local valid_neighbors = {}
  for _, dir in ipairs(directions) do
    local neighbor = coord + dir
    if is_valid_neighbor_func(grid, neighbor, dir) then
      table.insert(valid_neighbors, neighbor)
    end
  end
  return valid_neighbors
end

local function get_edges_bfs(grid, start_coord, vertices_set, is_valid_neighbor_func)
    local start_str = coord_to_string(start_coord)
    local frontier = {start_coord}
    local reached = {[start_str] = true}
    local distances = {[start_str] = 0}
    local edges = {} -- { neighbor_node_str = weight }

    local head = 1
    while head <= #frontier do
        local current_coord = frontier[head]
        head = head + 1
        local current_str = coord_to_string(current_coord)

        for _, next_coord in ipairs(neighbors4(grid, current_coord, is_valid_neighbor_func)) do
            local next_str = coord_to_string(next_coord)
            if not reached[next_str] then
                reached[next_str] = true
                distances[next_str] = distances[current_str] + 1
                if vertices_set[next_str] then
                    -- Found another graph vertex
                    edges[next_str] = distances[next_str]
                else
                    -- Continue BFS along the path
                    table.insert(frontier, next_coord)
                end
            end
        end
    end
    return edges
end


local function get_graph(grid, start_coord, end_coord, is_valid_neighbor_func)
    local graph = Graph:new()
    local start_str = coord_to_string(start_coord)
    local end_str = coord_to_string(end_coord)

    graph.vertices[start_str] = true
    graph.vertices[end_str] = true

    -- Find junction points (potential vertices)
    for y = 0, grid.height - 1 do
        for x = 0, grid.width - 1 do
            local coord = Coord:new(x, y)
            local coord_str = coord_to_string(coord)
            if not grid.data[coord_str] or grid.data[coord_str] ~= Wall then -- Check if not wall or empty
                if #neighbors4(grid, coord, is_valid_neighbor) > 2 then
                    graph.vertices[coord_str] = true
                end
            end
        end
    end

    -- Find edges between vertices using BFS
    for vertex_str in pairs(graph.vertices) do
        local vertex_coord = string_to_coord(vertex_str)
        local edges_from_vertex = get_edges_bfs(grid, vertex_coord, graph.vertices, is_valid_neighbor_func)
        graph.edges[vertex_str] = edges_from_vertex
    end

    return graph
end


local max_dist_dfs -- Declare forward for mutual recursion if needed (though not strictly here)
max_dist_dfs = function(graph, current_str, end_str, seen)
    if current_str == end_str then
        return true, 0
    end

    local maxi = -1 -- Use -1 to indicate no path found yet
    seen[current_str] = true

    if graph.edges[current_str] then
        for neighbor_str, weight in pairs(graph.edges[current_str]) do
            if not seen[neighbor_str] then
                local is_valid, dist = max_dist_dfs(graph, neighbor_str, end_str, seen)
                if is_valid then
                    if maxi == -1 or dist + weight > maxi then
                        maxi = dist + weight
                    end
                end
            end
        end
    end

    seen[current_str] = nil -- Backtrack

    if maxi == -1 then
        return false, 0 -- No path found from this node
    else
        return true, maxi
    end
end


local function solve(lines)
    local grid = parse_input(lines)

    local start_coord = Coord:new(1, 0)
    local end_coord = Coord:new(grid.width - 2, grid.height - 1)

    local graph = get_graph(grid, start_coord, end_coord, is_valid_neighbor)

    local _, max_dist = max_dist_dfs(graph, coord_to_string(start_coord), coord_to_string(end_coord), {})
    return max_dist
end

local function read_file(file_name)
    local file = io.open(file_name, "r")
    if not file then
        error("Could not open file: " .. file_name)
    end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()
    return lines
end

local function main()
    local lines = read_file("input.txt")
    local result = solve(lines)
    print(result)
end

main()
