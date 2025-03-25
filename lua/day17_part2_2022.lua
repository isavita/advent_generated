
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

local function can_move(rock, direction, chamber)
    local moved_rock = {}
    for i = 1, #rock do
        local x, y = rock[i][1], rock[i][2]
        local new_x, new_y
        if direction == 'left' then
            new_x, new_y = x - 1, y
        elseif direction == 'right' then
            new_x, new_y = x + 1, y
        elseif direction == 'down' then
            new_x, new_y = x, y - 1
        end
        
        if new_x < 0 or new_x > 6 or new_y < 1 or chamber[new_x*100000 + new_y] then
            return nil
        end
        moved_rock[#moved_rock + 1] = {new_x, new_y}
    end
    return moved_rock
end
local function get_chamber_profile(chamber, highest_y, depth)
    depth = depth or 30
    local profile = {}
    for x = 0, 6 do
        local found = false
        for y = highest_y, highest_y - depth, -1 do
            if chamber[x*100000 + y] then
                profile[#profile + 1] = highest_y - y
                found = true
                break
            end
        end
        if not found then
             profile[#profile + 1] = highest_y+1
        end
    end
    return profile
end

local function simulate(jet_pattern, total_rocks)
    local rock_shapes = get_rock_shapes()
    local chamber = {}
    for x = 0, 6 do
        chamber[x*100000] = true
    end
    
    local highest_y = 0
    local jet_len = #jet_pattern
    local jet_index = 0
    local rock_index = 0
    local seen_states = {}
    local additional_height = 0
    local rock_number = 0

    while rock_number < total_rocks do
        local shape = rock_shapes[(rock_index % #rock_shapes) + 1]
        local rock_x = 2
        local rock_y = highest_y + 4
        local rock = {}
        for i = 1, #shape do
            rock[i] = {rock_x + shape[i][1], rock_y + shape[i][2]}
        end

        while true do
            local jet_dir = jet_pattern:sub((jet_index % jet_len) + 1, (jet_index % jet_len) + 1)
            jet_index = jet_index + 1

            if jet_dir == '>' then
                local moved_rock = can_move(rock, 'right', chamber)
                if moved_rock then
                    rock = moved_rock
                end
            elseif jet_dir == '<' then
                local moved_rock = can_move(rock, 'left', chamber)
                if moved_rock then
                    rock = moved_rock
                end
            end

            local moved_down = can_move(rock, 'down', chamber)
            if moved_down then
                rock = moved_down
            else
                for i = 1, #rock do
                    local pos_x, pos_y = rock[i][1], rock[i][2]
                    chamber[pos_x*100000 + pos_y] = true
                    if pos_y > highest_y then
                        highest_y = pos_y
                    end
                end
                break
            end
        end
          local profile = get_chamber_profile(chamber, highest_y)
        local state_key = (rock_index % #rock_shapes) * (jet_len+1) * 1000
        for i=1, #profile do
            state_key = state_key + profile[i]
        end
        state_key = state_key + (jet_index % jet_len)
        
        if seen_states[state_key] and rock_number >= 2022 then
            local prev_rock, prev_height = seen_states[state_key][1], seen_states[state_key][2]
            local cycle_length = rock_number - prev_rock
            local cycle_height = highest_y - prev_height

            local remaining_rocks = total_rocks - rock_number
            local num_cycles = math.floor(remaining_rocks / cycle_length)

            additional_height = additional_height + num_cycles * cycle_height
            rock_number = rock_number + num_cycles * cycle_length
        else
            seen_states[state_key] = {rock_number, highest_y}
        end
        
        
        rock_number = rock_number + 1
        rock_index = rock_index + 1
    end

    return highest_y + additional_height
end

local function main()
    local jet_pattern = read_input("input.txt")
    local total_rocks = 1000000000000
    local final_height = simulate(jet_pattern, total_rocks)
    print(final_height)
end

main()
