
--[[
Lua solution for Advent of Code 2021 Day 19: Beacon Scanner

Reads scanner reports from input.txt, determines the relative positions
and orientations of scanners, builds a composite map of all beacons,
and prints the total number of unique beacons to standard output.
]]

-- Helper function for vector addition
local function vec_add(p1, p2)
    return { x = p1.x + p2.x, y = p1.y + p2.y, z = p1.z + p2.z }
end

-- Helper function for vector subtraction
local function vec_diff(p1, p2)
    return { x = p1.x - p2.x, y = p1.y - p2.y, z = p1.z - p2.z }
end

-- Helper function to convert a vector point to a string key for sets
local function vec_to_string(p)
    return string.format("%d,%d,%d", p.x, p.y, p.z)
end

-- Table storing the 24 possible orientation transformation functions
local orientations = {
    function(p) return { x =  p.x, y =  p.y, z =  p.z } end, -- Facing +x
    function(p) return { x =  p.x, y = -p.z, z =  p.y } end,
    function(p) return { x =  p.x, y = -p.y, z = -p.z } end,
    function(p) return { x =  p.x, y =  p.z, z = -p.y } end,
    function(p) return { x = -p.x, y = -p.y, z =  p.z } end, -- Facing -x
    function(p) return { x = -p.x, y =  p.z, z =  p.y } end,
    function(p) return { x = -p.x, y =  p.y, z = -p.z } end,
    function(p) return { x = -p.x, y = -p.z, z = -p.y } end,
    function(p) return { x =  p.y, y = -p.x, z =  p.z } end, -- Facing +y
    function(p) return { x =  p.y, y =  p.z, z =  p.x } end,
    function(p) return { x =  p.y, y =  p.x, z = -p.z } end,
    function(p) return { x =  p.y, y = -p.z, z = -p.x } end,
    function(p) return { x = -p.y, y =  p.x, z =  p.z } end, -- Facing -y
    function(p) return { x = -p.y, y = -p.z, z =  p.x } end,
    function(p) return { x = -p.y, y = -p.x, z = -p.z } end,
    function(p) return { x = -p.y, y =  p.z, z = -p.x } end,
    function(p) return { x =  p.z, y =  p.y, z = -p.x } end, -- Facing +z
    function(p) return { x =  p.z, y =  p.x, z =  p.y } end,
    function(p) return { x =  p.z, y = -p.y, z =  p.x } end,
    function(p) return { x =  p.z, y = -p.x, z = -p.y } end,
    function(p) return { x = -p.z, y = -p.y, z = -p.x } end, -- Facing -z
    function(p) return { x = -p.z, y = -p.x, z =  p.y } end,
    function(p) return { x = -p.z, y =  p.y, z =  p.x } end,
    function(p) return { x = -p.z, y =  p.x, z = -p.y } end,
}

-- Parses the input file into a list of scanners, each with a list of beacon coordinates
local function parse_input(filename)
    local scanners = {}
    local current_scanner_beacons = nil
    local file = io.open(filename, "r")
    if not file then
        error("Could not open file: " .. filename)
    end

    for line in file:lines() do
        if line:match("^--- scanner %d+ ---$") then
            current_scanner_beacons = {}
            table.insert(scanners, { beacons = current_scanner_beacons })
        elseif line:match("^(-?%d+),(-?%d+),(-?%d+)$") then
            local x, y, z = line:match("^(-?%d+),(-?%d+),(-?%d+)$")
            table.insert(current_scanner_beacons, { x = tonumber(x), y = tonumber(y), z = tonumber(z) })
        end
    end
    file:close()
    return scanners
end

-- Tries to find an overlap between a known scanner and an unknown scanner
-- known_scanner_info: Table containing { abs_pos, orientation_idx, abs_beacons, abs_beacons_set } for the known scanner
-- unknown_scanner_data: Table containing { beacons } for the unknown scanner (relative coordinates)
-- Returns: success (boolean), abs_pos (vector), orientation_idx (number), abs_beacons (list) if successful, otherwise false
local function try_match(known_scanner_info, unknown_scanner_data)
    local known_beacons_abs = known_scanner_info.abs_beacons
    local known_beacons_abs_set = known_scanner_info.abs_beacons_set
    local unknown_beacons_relative = unknown_scanner_data.beacons

    for ori_idx = 1, #orientations do
        local transform_func = orientations[ori_idx]

        -- Try every pair of beacons (one known, one unknown) to hypothesize an offset
        for i = 1, #known_beacons_abs do
            local known_b = known_beacons_abs[i]

            for j = 1, #unknown_beacons_relative do
                -- Assume unknown_beacons_relative[j] corresponds to known_b
                local oriented_unknown_b = transform_func(unknown_beacons_relative[j])
                -- Calculate the hypothetical absolute position of the unknown scanner
                -- unknown_scanner_pos = known_b - oriented_unknown_b
                local delta = vec_diff(known_b, oriented_unknown_b)

                -- Verify this hypothesis: Check how many beacons match using this orientation and delta
                local match_count = 0
                for k = 1, #unknown_beacons_relative do
                    local oriented_b_k = transform_func(unknown_beacons_relative[k])
                    -- Calculate potential absolute position of this beacon
                    local potential_abs_b = vec_add(oriented_b_k, delta)
                    local key = vec_to_string(potential_abs_b)

                    if known_beacons_abs_set[key] then
                        match_count = match_count + 1
                    end
                end

                -- Check if we found at least 12 matches
                if match_count >= 12 then
                    -- Success! We found the orientation and position
                    local unknown_scanner_abs_pos = delta -- The offset *is* the scanner position relative to 0,0,0

                    -- Calculate absolute positions of all beacons for this unknown scanner
                    local unknown_beacons_abs = {}
                    for k = 1, #unknown_beacons_relative do
                        local oriented_b = transform_func(unknown_beacons_relative[k])
                        unknown_beacons_abs[k] = vec_add(oriented_b, unknown_scanner_abs_pos)
                    end

                    return true, unknown_scanner_abs_pos, ori_idx, unknown_beacons_abs
                end
            end
        end
    end

    -- No match found for any orientation/offset
    return false
end

-- Main execution function
local function main()
    local scanners_data = parse_input("input.txt")
    local num_scanners = #scanners_data

    -- Set to store unique beacon absolute coordinates ("x,y,z" strings)
    local all_beacons_set = {}
    -- Table to store information about located scanners (indexed 0 to num_scanners-1)
    -- scanner_info[idx] = { abs_pos = {...}, orientation_idx = ..., abs_beacons = {...}, abs_beacons_set = {...} }
    local scanner_info = {}
    -- Set to keep track of located scanner indices
    local located_indices = {}
    -- Queue for Breadth-First Search (BFS) of scanners to process
    local queue = {}

    -- Initialize with scanner 0 as the reference point
    local initial_scanner_idx = 0
    scanner_info[initial_scanner_idx] = {
        abs_pos = { x = 0, y = 0, z = 0 },
        orientation_idx = 1, -- Identity orientation
        abs_beacons = {},
        abs_beacons_set = {}
    }
    -- Scanner data is 1-based, internal indices are 0-based
    for _, beacon in ipairs(scanners_data[initial_scanner_idx + 1].beacons) do
        -- For scanner 0, relative coords are absolute coords
        local abs_beacon = { x = beacon.x, y = beacon.y, z = beacon.z }
        table.insert(scanner_info[initial_scanner_idx].abs_beacons, abs_beacon)
        local key = vec_to_string(abs_beacon)
        scanner_info[initial_scanner_idx].abs_beacons_set[key] = true
        all_beacons_set[key] = true -- Add to the global set
    end

    located_indices[initial_scanner_idx] = true
    table.insert(queue, initial_scanner_idx)
    local located_count = 1

    -- BFS to find and align all scanners
    while #queue > 0 and located_count < num_scanners do
        local known_idx = table.remove(queue, 1) -- Dequeue

        -- Try to match this known scanner against all currently unknown scanners
        for unknown_idx = 0, num_scanners - 1 do
            if not located_indices[unknown_idx] then
                --print(string.format("Attempting match: known %d vs unknown %d", known_idx, unknown_idx))
                local success, abs_pos, ori_idx, abs_beacons = try_match(
                    scanner_info[known_idx],
                    scanners_data[unknown_idx + 1] -- Use 1-based index for original data
                )

                if success then
                    --print(string.format("  -> Match Found! Scanner %d aligned.", unknown_idx))
                    located_indices[unknown_idx] = true
                    located_count = located_count + 1
                    table.insert(queue, unknown_idx) -- Enqueue the newly located scanner

                    -- Store information for the newly located scanner
                    local abs_beacons_set = {}
                    for _, beacon in ipairs(abs_beacons) do
                        local key = vec_to_string(beacon)
                        abs_beacons_set[key] = true
                        all_beacons_set[key] = true -- Add to global set
                    end

                    scanner_info[unknown_idx] = {
                        abs_pos = abs_pos,
                        orientation_idx = ori_idx,
                        abs_beacons = abs_beacons,
                        abs_beacons_set = abs_beacons_set
                    }
                    -- Optimization: Since we found a match for unknown_idx, we might
                    -- not need to check it against other known scanners in this pass,
                    -- but the BFS structure handles this naturally.
                end
            end
        end
    end

    -- Count the total number of unique beacons found
    local unique_beacon_count = 0
    for _ in pairs(all_beacons_set) do
        unique_beacon_count = unique_beacon_count + 1
    end

    -- Print the final result
    print(unique_beacon_count)
end

-- Run the main function
main()
