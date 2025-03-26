
--[[
    Day 23: Unstable Diffusion
    Simulates elf movement on an infinite grid based on neighbor checks and rotating direction priorities.
    Calculates the empty space in the bounding box after 10 rounds (Part 1)
    and the first round where no elf moves (Part 2).
]]

-- Helper function to parse a coordinate string "x,y" into numbers
local function parse_coord(coord_str)
    local x, y = coord_str:match("^(-?%d+),(-?%d+)$")
    return tonumber(x), tonumber(y)
end

-- Helper function to create a coordinate string
local function format_coord(x, y)
    return x .. "," .. y
end

-- Helper function to count keys in a table (acting as a set)
local function count_table_keys(t)
    local count = 0
    for _ in pairs(t) do
        count = count + 1
    end
    return count
end

-- Parses the input file into a set of elf coordinates (keys are "x,y", value is true)
local function parse_input(filename)
    local file = io.open(filename, "r")
    if not file then
        error("Could not open file: " .. filename)
    end

    local elves = {}
    local y = 0
    for line in file:lines() do
        for x = 1, #line do
            if line:sub(x, x) == '#' then
                elves[format_coord(x - 1, y)] = true -- Use 0-based indexing internally
            end
        end
        y = y + 1
    end
    file:close()
    return elves
end

-- Precompute neighbor offsets for faster checking
local neighbor_offsets = {
    {-1, -1}, { 0, -1}, { 1, -1}, -- NW, N, NE
    {-1,  0},           { 1,  0}, -- W, E
    {-1,  1}, { 0,  1}, { 1,  1}  -- SW, S, SE
}

-- Define the checks for each direction [ {target_offset}, {diag1_offset}, {diag2_offset} ]
-- The target cell is always the first offset in the inner table.
local direction_checks = {
    N = {{ 0, -1}, {-1, -1}, { 1, -1}}, -- N, NW, NE
    S = {{ 0,  1}, {-1,  1}, { 1,  1}}, -- S, SW, SE
    W = {{-1,  0}, {-1, -1}, {-1,  1}}, -- W, NW, SW
    E = {{ 1,  0}, { 1, -1}, { 1,  1}}  -- E, NE, SE
}

-- Simulates one round of elf movement
-- Returns the new set of elf positions and the number of elves that moved
local function run_round(elves, current_directions)
    local proposed_moves = {} -- { ["src_x,src_y"] = "dest_x,dest_y" }
    local target_counts = {}  -- { ["dest_x,dest_y"] = count }

    -- 1. Proposal Phase
    for elf_coord in pairs(elves) do
        local x, y = parse_coord(elf_coord)

        -- Check if any neighbors exist
        local has_neighbors = false
        for _, offset in ipairs(neighbor_offsets) do
            local nx, ny = x + offset[1], y + offset[2]
            if elves[format_coord(nx, ny)] then
                has_neighbors = true
                break
            end
        end

        if has_neighbors then
            -- Consider moving in the specified order
            for _, dir in ipairs(current_directions) do
                local checks = direction_checks[dir]
                local can_propose = true
                local target_x, target_y

                for i, offset in ipairs(checks) do
                    local check_x, check_y = x + offset[1], y + offset[2]
                    if elves[format_coord(check_x, check_y)] then
                        can_propose = false
                        break
                    end
                    if i == 1 then -- First offset is the target cell
                        target_x, target_y = check_x, check_y
                    end
                end

                if can_propose then
                    local target_coord = format_coord(target_x, target_y)
                    proposed_moves[elf_coord] = target_coord
                    target_counts[target_coord] = (target_counts[target_coord] or 0) + 1
                    break -- Propose the first valid direction
                end
            end
        end
        -- If no neighbors or no valid proposal, the elf stays put (implicitly)
    end

    -- 2. Movement Phase
    local new_elves = {}
    local moved_count = 0
    for elf_coord in pairs(elves) do
        local target_coord = proposed_moves[elf_coord]

        -- Move if a proposal exists and it's unique
        if target_coord and target_counts[target_coord] == 1 then
            new_elves[target_coord] = true
            moved_count = moved_count + 1
        else
            -- Stay in the original position
            new_elves[elf_coord] = true
        end
    end

    return new_elves, moved_count
end

-- Calculates the bounding box and empty tiles for Part 1
local function calculate_part1_result(elves)
    if count_table_keys(elves) == 0 then return 0 end

    local min_x, max_x = math.huge, -math.huge
    local min_y, max_y = math.huge, -math.huge

    for elf_coord in pairs(elves) do
        local x, y = parse_coord(elf_coord)
        min_x = math.min(min_x, x)
        max_x = math.max(max_x, x)
        min_y = math.min(min_y, y)
        max_y = math.max(max_y, y)
    end

    local width = max_x - min_x + 1
    local height = max_y - min_y + 1
    local total_tiles = width * height
    local num_elves = count_table_keys(elves)

    return total_tiles - num_elves
end

-- Main execution function
local function main()
    local elves = parse_input("input.txt")
    -- Initial order of directions to check
    local directions = {"N", "S", "W", "E"}
    local round_number = 0
    local part1_result = nil

    while true do
        round_number = round_number + 1

        local new_elves, moved_count
        new_elves, moved_count = run_round(elves, directions)
        elves = new_elves

        -- Part 1: Calculate result after round 10
        if round_number == 10 then
            part1_result = calculate_part1_result(elves)
        end

        -- Part 2: Stop when no elves move
        if moved_count == 0 then
            -- Ensure Part 1 result is printed if the simulation ends before or at round 10
            if not part1_result then
                 part1_result = calculate_part1_result(elves)
            end
            print("Part 1:", part1_result)
            print("Part 2:", round_number)
            break
        end

        -- Rotate directions for the next round
        table.insert(directions, table.remove(directions, 1))

        -- Safety break for extremely long runs (optional)
        -- if round_number > 2000 then
        --     print("Reached round limit, stopping.")
        --     if part1_result then print("Part 1:", part1_result) end
        --     break
        -- end
    end
end

-- Run the simulation
main()
