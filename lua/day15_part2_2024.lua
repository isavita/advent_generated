
local function coord(x, y)
    return { x = x, y = y }
end

local function coord_add(c1, c2)
    return coord(c1.x + c2.x, c1.y + c2.y)
end

local function coord_mul(k, c)
    return coord(k * c.x, k * c.y)
end

local function coord_key(c)
    return c.x .. "," .. c.y
end

local function coord_eq(c1, c2)
    return c1.x == c2.x and c1.y == c2.y
end

local function get_grid(grid, c)
    return grid[coord_key(c)]
end

local function set_grid(grid, c, value)
    grid[coord_key(c)] = value
end

local function deepcopy_grid(orig)
    local copy = {}
    for k, v in pairs(orig) do
        copy[k] = v
    end
    return copy
end

local function try_to_step(m, target_pos, dir)
    local orig_m = deepcopy_grid(m)
    local success = false
    local char_at_target = get_grid(m, target_pos)

    if char_at_target == '.' then
        success = true
    elseif char_at_target == 'O' or char_at_target == '@' then
        local pushed_obj_target_pos = coord_add(target_pos, dir)
        if try_to_step(m, pushed_obj_target_pos, dir) then
            set_grid(m, pushed_obj_target_pos, char_at_target)
            set_grid(m, target_pos, '.')
            success = true
        end
    elseif char_at_target == ']' then
        local pos_left_of_bracket = coord_add(target_pos, coord(-1, 0))
        if try_to_step(m, pos_left_of_bracket, dir) then
            success = true
        end
    elseif char_at_target == '[' then
        local pos_right_of_bracket = coord_add(target_pos, coord(1, 0))

        if coord_eq(dir, coord(-1, 0)) then
            local new_bracket_pos = coord_add(target_pos, dir)
            if try_to_step(m, new_bracket_pos, dir) then
                set_grid(m, new_bracket_pos, '[')
                set_grid(m, target_pos, ']')
                set_grid(m, pos_right_of_bracket, '.')
                success = true
            end
        elseif coord_eq(dir, coord(1, 0)) then
            local new_bracket_pos = coord_add(target_pos, coord(2, 0))
            if try_to_step(m, new_bracket_pos, dir) then
                set_grid(m, target_pos, '.')
                set_grid(m, pos_right_of_bracket, '[')
                set_grid(m, new_bracket_pos, ']')
                success = true
            end
        else
            local new_bracket_pos = coord_add(target_pos, dir)
            local new_end_bracket_pos = coord_add(pos_right_of_bracket, dir)
            if try_to_step(m, new_bracket_pos, dir) and try_to_step(m, new_end_bracket_pos, dir) then
                 set_grid(m, target_pos, '.')
                 set_grid(m, pos_right_of_bracket, '.')
                 set_grid(m, new_bracket_pos, '[')
                 set_grid(m, new_end_bracket_pos, ']')
                 success = true
             end
         end
     end

     if not success then
         for k in pairs(m) do m[k] = nil end
         for k, v in pairs(orig_m) do m[k] = v end
     end

     return success
end

local function solve(input_str)
    local blocks = {}
    local i = 1
    local j = input_str:find("\n\n", i, true)
    while j do
        table.insert(blocks, input_str:sub(i, j-1))
        i = j + 2
        j = input_str:find("\n\n", i, true)
    end
    table.insert(blocks, input_str:sub(i))

    local grid_str = blocks[1]
    local step_str = blocks[2]:gsub("\n", "")

    local m = {}
    local robot_pos = nil

    local lines = {}
    i = 1
    j = grid_str:find("\n", i, true)
    while j do
         table.insert(lines, grid_str:sub(i, j-1))
         i = j + 1
         j = grid_str:find("\n", i, true)
     end
    table.insert(lines, grid_str:sub(i))

    for y = 1, #lines do
        local row = lines[y]
        for x = 0, #row - 1 do
            local char = row:sub(x+1, x+1)
            local p = coord(x, y-1)
            set_grid(m, p, char)
            if char == '@' then
                robot_pos = p
            end
        end
    end

    local steps = {}
    for i = 1, #step_str do
        local char = step_str:sub(i, i)
        if char == '^' then table.insert(steps, coord(0, -1))
        elseif char == '<' then table.insert(steps, coord(-1, 0))
        elseif char == '>' then table.insert(steps, coord(1, 0))
        elseif char == 'v' then table.insert(steps, coord(0, 1))
        end
    end

    for _, dir in ipairs(steps) do
        local target_pos = coord_add(robot_pos, dir)
        if try_to_step(m, target_pos, dir) then
             local robot_char = get_grid(m, robot_pos)
             set_grid(m, target_pos, robot_char)
             set_grid(m, robot_pos, '.')
             robot_pos = target_pos
        end
    end

    local score = 0
    for key, char in pairs(m) do
        if char == '[' or char == 'O' then
            local sx, sy = key:match("(-?%d+),(-?%d+)")
            local kx = tonumber(sx)
            local ky = tonumber(sy)
            score = score + (kx + 100 * ky)
        end
    end

    return score
end

local function scale_up(input_str)
    local s = input_str:gsub("#", "##")
    s = s:gsub("%.", "..")
    s = s:gsub("O", "[]")
    s = s:gsub("@", "@.")
    return s
end

local function main()
    local file = io.open("input.txt", "r")
    if not file then
        io.stderr:write("Error: Could not open input.txt\n")
        return
    end
    local input_str = file:read("*a")
    file:close()

    local score1 = solve(input_str)
    print(string.format("%.0f", score1))

    local scaled_input_str = scale_up(input_str)
    local score2 = solve(scaled_input_str)
    print(string.format("%.0f", score2))
end

main()
