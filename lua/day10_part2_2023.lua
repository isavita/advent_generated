
--[[
--- Day 10: Pipe Maze ---

Solution approach:
Part 1:
1. Read the grid from input.txt.
2. Find the starting position 'S'.
3. Perform a Breadth-First Search (BFS) starting from 'S' to find all reachable pipe tiles belonging to the main loop.
4. Keep track of the distance from 'S' for each tile during the BFS.
5. The maximum distance found during the BFS is the answer to Part 1. We also need to store the loop path for Part 2.

Part 2:
1. Identify the actual pipe shape at the 'S' position based on its connected neighbors in the loop. Replace 'S' in the grid with this shape.
2. Use the loop path identified in Part 1.
3. Iterate through each tile of the grid.
4. For tiles *not* part of the loop, determine if they are inside or outside the loop using the ray casting algorithm (scanline approach).
5. Scan each row. Maintain an "inside" state (boolean). When encountering a vertical segment of the loop pipe (`|`, `L`, `J`), toggle the "inside" state.
6. If a non-loop tile is encountered while the state is "inside", increment the count of enclosed tiles.

Data Structures:
- Grid: table of tables (rows) of characters.
- Distances: table of tables storing distances from 'S' (-1 for unvisited/not on loop).
- Queue: table for BFS.

Helper Functions:
- `read_input`: Reads the file and builds the grid.
- `find_start`: Locates 'S'.
- `get_connections`: Returns possible moves from a given pipe type.
- `is_valid`: Checks if coordinates are within grid bounds.
- `determine_start_pipe`: Figures out the actual pipe type for 'S'.
- `bfs`: Performs the BFS for Part 1 and identifies loop tiles.
- `count_enclosed`: Implements the scanline algorithm for Part 2.
--]]

local M = {}

-- Check if coordinates are within grid bounds (1-based indexing)
function M.is_valid(x, y, width, height)
    return x >= 1 and x <= width and y >= 1 and y <= height
end

-- Get potential connections for a pipe character
-- Returns {dx1, dy1, dx2, dy2} or nil
function M.get_delta_connections(pipe)
    if pipe == '|' then return {0, -1, 0, 1} end -- N, S
    if pipe == '-' then return {-1, 0, 1, 0} end -- W, E
    if pipe == 'L' then return {0, -1, 1, 0} end -- N, E
    if pipe == 'J' then return {0, -1, -1, 0} end -- N, W
    if pipe == '7' then return {-1, 0, 0, 1} end -- W, S
    if pipe == 'F' then return {1, 0, 0, 1} end -- E, S
    if pipe == '.' then return nil end
    if pipe == 'S' then return nil end -- Handle S separately initially
    return nil
end

-- Check if pipe2 at (x2, y2) connects back to (x1, y1)
function M.connects_back(pipe2, x1, y1, x2, y2)
    local deltas = M.get_delta_connections(pipe2)
    if not deltas then return false end

    local dx_target, dy_target = x1 - x2, y1 - y2 -- Required delta from pipe2 to pipe1

    for i = 1, #deltas, 2 do
        if deltas[i] == dx_target and deltas[i+1] == dy_target then
            return true
        end
    end
    return false
end


-- Determine the actual pipe type for 'S' based on neighbors
function M.determine_start_pipe(grid, sx, sy, width, height)
    local connects_north = false
    local connects_south = false
    local connects_west = false
    local connects_east = false

    -- Check North
    if M.is_valid(sx, sy - 1, width, height) then
        local north_pipe = grid[sy - 1][sx]
        if north_pipe == '|' or north_pipe == '7' or north_pipe == 'F' then
            connects_north = true
        end
    end
    -- Check South
    if M.is_valid(sx, sy + 1, width, height) then
        local south_pipe = grid[sy + 1][sx]
        if south_pipe == '|' or south_pipe == 'L' or south_pipe == 'J' then
            connects_south = true
        end
    end
    -- Check West
    if M.is_valid(sx - 1, sy, width, height) then
        local west_pipe = grid[sy][sx - 1]
        if west_pipe == '-' or west_pipe == 'L' or west_pipe == 'F' then
            connects_west = true
        end
    end
    -- Check East
    if M.is_valid(sx + 1, sy, width, height) then
        local east_pipe = grid[sy][sx + 1]
        if east_pipe == '-' or east_pipe == 'J' or east_pipe == '7' then
            connects_east = true
        end
    end

    if connects_north and connects_south then return '|' end
    if connects_east and connects_west then return '-' end
    if connects_north and connects_east then return 'L' end
    if connects_north and connects_west then return 'J' end
    if connects_south and connects_west then return '7' end
    if connects_south and connects_east then return 'F' end

    error("Could not determine start pipe type for 'S'")
end

-- Perform BFS to find loop and distances
function M.bfs(grid, start_x, start_y, width, height)
    local distances = {}
    for y = 1, height do
        distances[y] = {}
        for x = 1, width do
            distances[y][x] = -1
        end
    end

    local queue = {}
    distances[start_y][start_x] = 0
    table.insert(queue, {x = start_x, y = start_y, dist = 0})

    local max_dist = 0
    local head = 1

    while head <= #queue do
        local current = queue[head]
        head = head + 1

        local x, y, dist = current.x, current.y, current.dist
        max_dist = math.max(max_dist, dist)
        local current_pipe = grid[y][x]

        -- Find initial neighbors for 'S' or neighbors for other pipes
         local potential_neighbors = {}
         if current_pipe == 'S' then
             -- Check all 4 directions for S
             local neighbors = {{0,-1}, {0,1}, {-1,0}, {1,0}} -- N, S, W, E
             for _, delta in ipairs(neighbors) do
                 local nx, ny = x + delta[1], y + delta[2]
                 if M.is_valid(nx, ny, width, height) then
                     local next_pipe = grid[ny][nx]
                     if M.connects_back(next_pipe, x, y, nx, ny) then
                         table.insert(potential_neighbors, {nx, ny})
                     end
                 end
             end
         else
             -- Get connections based on pipe type
             local deltas = M.get_delta_connections(current_pipe)
             if deltas then
                 for i = 1, #deltas, 2 do
                      local nx, ny = x + deltas[i], y + deltas[i+1]
                      table.insert(potential_neighbors, {nx, ny})
                 end
             end
         end

         -- Process valid neighbors
         for _, neighbor_coords in ipairs(potential_neighbors) do
             local nx, ny = neighbor_coords[1], neighbor_coords[2]
             if M.is_valid(nx, ny, width, height) and distances[ny][nx] == -1 then
                 local next_pipe = grid[ny][nx]
                 -- Ensure the next pipe actually connects back to the current one
                 if M.connects_back(next_pipe, x, y, nx, ny) then
                      distances[ny][nx] = dist + 1
                      table.insert(queue, {x = nx, y = ny, dist = dist + 1})
                 -- Special handling needed if starting from S, as first step doesn't need connects_back
                 elseif current_pipe == 'S' then
                     distances[ny][nx] = dist + 1
                     table.insert(queue, {x = nx, y = ny, dist = dist + 1})
                 end

             end
         end
    end

    return max_dist, distances
end

-- Count enclosed tiles using scanline algorithm
function M.count_enclosed(grid, distances, width, height)
    local enclosed_count = 0

    for y = 1, height do
        local inside = false
        for x = 1, width do
            if distances[y][x] ~= -1 then -- Tile is part of the loop
                local pipe = grid[y][x]
                -- Toggle 'inside' state only for pipes that cross the scanline vertically
                -- We consider '|', 'L', and 'J' as crossing from top to bottom
                -- when scanning left-to-right. F---7 doesn't cross, L---J does.
                if pipe == '|' or pipe == 'L' or pipe == 'J' then
                    inside = not inside
                end
            else -- Tile is not part of the loop
                if inside then
                    enclosed_count = enclosed_count + 1
                end
            end
        end
    end

    return enclosed_count
end


-- Main execution function
function M.main()
    local filename = "input.txt"
    local file = io.open(filename, "r")
    if not file then
        error("Could not open file: " .. filename)
    end

    local grid = {}
    local start_x, start_y = -1, -1
    local y = 1
    for line in file:lines() do
        local row = {}
        for x = 1, #line do
            local char = line:sub(x, x)
            row[x] = char
            if char == 'S' then
                start_x, start_y = x, y
            end
        end
        table.insert(grid, row)
        y = y + 1
    end
    file:close()

    local height = #grid
    local width = #grid[1]

    -- Part 1: Find the loop and the farthest distance
    local max_dist, distances = M.bfs(grid, start_x, start_y, width, height)
    print("Part 1:", max_dist)

    -- Part 2: Count enclosed tiles
    -- First, determine the actual pipe at 'S' and replace it in the grid copy
    local start_pipe_type = M.determine_start_pipe(grid, start_x, start_y, width, height)
    grid[start_y][start_x] = start_pipe_type -- Update the grid for scanline

    local enclosed_count = M.count_enclosed(grid, distances, width, height)
    print("Part 2:", enclosed_count)

end

-- Run the main function
M.main()
