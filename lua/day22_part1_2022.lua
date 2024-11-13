
-- Read input from file
local function read_input()
    local map = {}
    local path = ""
    local map_done = false

    for line in io.lines("input.txt") do
        if line == "" then
            map_done = true
        elseif not map_done then
            table.insert(map, line)
        else
            path = line
        end
    end

    return map, path
end

-- Find the starting position
local function find_start(map)
    for j = 1, #map[1] do
        if map[1]:sub(j,j) == '.' then
            return 1, j
        end
    end
end

-- Check if a position is valid
local function is_valid_pos(map, row, col)
    return row > 0 and row <= #map and 
           col > 0 and col <= #map[row] and 
           map[row]:sub(col,col) ~= ' '
end

-- Wrap around the map
local function wrap_around(map, row, col, dir)
    local dr, dc = dir[1], dir[2]
    local new_row, new_col = row, col

    -- Move in opposite direction until edge
    while is_valid_pos(map, new_row - dr, new_col - dc) do
        new_row = new_row - dr
        new_col = new_col - dc
    end

    return new_row, new_col
end

-- Main solution
local function solve()
    local map, path = read_input()
    local row, col = find_start(map)
    
    -- Directions: right, down, left, up
    local directions = {{0,1}, {1,0}, {0,-1}, {-1,0}}
    local dir_index = 1  -- Start facing right

    local i = 1
    while i <= #path do
        -- Parse movement
        local steps
        local j = path:find("%D", i)
        if j then
            steps = tonumber(path:sub(i, j-1))
            i = j
        else
            steps = tonumber(path:sub(i))
            i = #path + 1
        end

        -- Move steps
        for _ = 1, steps do
            local new_row = row + directions[dir_index][1]
            local new_col = col + directions[dir_index][2]

            -- Check if move is valid
            if not is_valid_pos(map, new_row, new_col) then
                new_row, new_col = wrap_around(map, row, col, directions[dir_index])
            end

            -- Check if new position is a wall
            if map[new_row]:sub(new_col,new_col) == '#' then
                break
            end

            row, col = new_row, new_col
        end

        -- Parse turn
        if i <= #path then
            local turn = path:sub(i,i)
            if turn == 'R' then
                dir_index = dir_index % 4 + 1
            else
                dir_index = (dir_index - 2 + 4) % 4 + 1
            end
            i = i + 1
        end
    end

    -- Calculate password
    local password = 1000 * row + 4 * col + (dir_index - 1)
    print(password)
end

solve()
