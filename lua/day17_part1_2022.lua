
local function read_input(filename)
    local file = io.open(filename, "r")
    local line = file:read("*line")
    file:close()
    return line
end

local function get_rock_shapes()
    return {
        {{0,0}, {1,0}, {2,0}, {3,0}},
        {{1,0}, {0,1}, {1,1}, {2,1}, {1,2}},
        {{0,0}, {1,0}, {2,0}, {2,1}, {2,2}},
        {{0,0}, {0,1}, {0,2}, {0,3}},
        {{0,0}, {1,0}, {0,1}, {1,1}}
    }
end

local function can_move(rock, direction, chamber, highest_y)
    local moved_rock = {}
    for i, pos in ipairs(rock) do
        local x, y = pos[1], pos[2]
        local new_x, new_y
        if direction == 'left' then
            new_x, new_y = x - 1, y
        elseif direction == 'right' then
            new_x, new_y = x + 1, y
        elseif direction == 'down' then
            new_x, new_y = x, y - 1
        end

        if new_x < 0 or new_x > 6 or new_y < 1 or chamber[new_x] and chamber[new_x][new_y] then
            return false
        end
        moved_rock[i] = {new_x, new_y}
    end
    return moved_rock
end

local function simulate(jet_pattern, total_rocks)
    local rock_shapes = get_rock_shapes()
    local chamber = {}
    for x = 0, 6 do
        chamber[x] = {}
        chamber[x][0] = true
    end

    local highest_y = 0
    local jet_len = #jet_pattern
    local jet_index = 1

    for rock_number = 0, total_rocks - 1 do
        local shape = rock_shapes[(rock_number % #rock_shapes) + 1]
        local rock_x = 2
        local rock_y = highest_y + 4
        local rock = {}
        for i, pos in ipairs(shape) do
            rock[i] = {rock_x + pos[1], rock_y + pos[2]}
        end

        while true do
            local jet_dir = jet_pattern:sub(jet_index, jet_index)
            jet_index = (jet_index % jet_len) + 1

            local moved_rock
            if jet_dir == '>' then
                moved_rock = can_move(rock, 'right', chamber, highest_y)
            elseif jet_dir == '<' then
                moved_rock = can_move(rock, 'left', chamber, highest_y)
            end
            if moved_rock then
                rock = moved_rock
            end

            local moved_down = can_move(rock, 'down', chamber, highest_y)
            if moved_down then
                rock = moved_down
            else
                for _, pos in ipairs(rock) do
                    local x, y = pos[1], pos[2]
                    if not chamber[x] then chamber[x] = {} end
                    chamber[x][y] = true
                    if y > highest_y then
                        highest_y = y
                    end
                end
                break
            end
        end
    end

    return highest_y
end

local function main()
    local jet_pattern = read_input('input.txt')
    local total_rocks = 2022
    local final_height = simulate(jet_pattern, total_rocks)
    print(final_height)
end

main()
