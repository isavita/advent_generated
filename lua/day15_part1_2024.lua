
local M = {}

local function solve()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening input.txt")
        return
    end

    local grid_lines = {}
    local moves = ""
    local reading_map = true

    for line in file:lines() do
        if reading_map then
            if line:find("#", 1, true) or line:find("@", 1, true) or line:find("O", 1, true) or line:find("%.", 1, true) then
                 if #line > 0 then
                    local row = {}
                    for i = 1, #line do
                        table.insert(row, line:sub(i, i))
                    end
                    table.insert(grid_lines, row)
                 else
                    reading_map = false -- Empty line signals end of map
                 end
            else -- Line doesn't contain map chars, must be separator or moves
                reading_map = false
                moves = moves .. line:match("^%s*(.-)%s*$") -- Trim whitespace
            end
        else
            moves = moves .. line:match("^%s*(.-)%s*$") -- Trim whitespace
        end
    end
    file:close()

    local grid = grid_lines
    local rows = #grid
    local cols = #grid[1]
    local robot_r, robot_c

    for r = 1, rows do
        for c = 1, cols do
            if grid[r][c] == '@' then
                robot_r, robot_c = r, c
                goto found_robot -- Use goto for early exit
            end
        end
    end
    ::found_robot::

    local dirs = {
        ['^'] = {-1, 0},
        ['v'] = {1, 0},
        ['<'] = {0, -1},
        ['>'] = {0, 1}
    }

    local function is_valid(r, c)
        return r >= 1 and r <= rows and c >= 1 and c <= cols
    end

    local push_boxes -- Declare recursive function beforehand

    push_boxes = function(r, c, dr, dc)
        local nr, nc = r + dr, c + dc
        if not is_valid(nr, nc) or grid[nr][nc] == '#' then
            return false
        end

        if grid[nr][nc] == 'O' then
            if not push_boxes(nr, nc, dr, dc) then
                return false
            end
            -- After successful recursive push, the spot (nr, nc) must be '.'
        end

        -- Now check if the target spot is clear (it should be '.')
        if grid[nr][nc] == '.' then
             grid[nr][nc] = 'O'
             grid[r][c] = '.'
             return true
        end
        -- If it's not '.', something went wrong or the logic implies failure
        return false
    end

    for i = 1, #moves do
        local move = moves:sub(i, i)
        local dir = dirs[move]
        if dir then
            local dr, dc = dir[1], dir[2]
            local nr, nc = robot_r + dr, robot_c + dc

            if is_valid(nr, nc) then
                local next_cell = grid[nr][nc]

                if next_cell == '#' then
                    -- Continue to next move
                elseif next_cell == 'O' then
                    if push_boxes(nr, nc, dr, dc) then
                        -- The push succeeded, the spot (nr, nc) is now '.'
                        grid[robot_r][robot_c] = '.'
                        grid[nr][nc] = '@'
                        robot_r, robot_c = nr, nc
                    else
                        -- Push failed, do nothing (continue to next move)
                    end
                elseif next_cell == '.' then
                    grid[robot_r][robot_c] = '.'
                    grid[nr][nc] = '@'
                    robot_r, robot_c = nr, nc
                end
            end
        end
    end

    local total_sum = 0
    for r = 1, rows do
        for c = 1, cols do
            if grid[r][c] == 'O' then
                -- Lua uses 1-based indexing, calculation needs 0-based
                total_sum = total_sum + (r - 1) * 100 + (c - 1)
            end
        end
    end

    print(total_sum)
end

function M.main()
    solve()
end

M.main()

return M
