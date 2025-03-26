
--[[
    Advent of Code 2023 - Day 21: Step Counter
    Lua solution focusing on clarity and efficiency.
]]

-- Helper function to parse string keys like "y,x" into numbers
local function parse_key(key)
    local parts = {}
    -- Use string.gmatch to split by comma
    for part in string.gmatch(key, "([^,]+)") do
        table.insert(parts, tonumber(part))
    end
    -- Ensure we got two parts
    if #parts == 2 then
        return parts[1], parts[2]
    else
        -- Return nil or handle error if the key format is invalid
        error("Invalid position key format: " .. key)
        -- return nil, nil
    end
end


--[[
    Performs a Breadth-First Search (BFS) to find reachable garden plots.
    Handles both finite (Part 1) and infinite (Part 2) grids.
    Returns the count of reachable plots after exactly `target_steps`.

    Parameters:
        grid (table): The 2D map (table of tables).
        height (number): Grid height.
        width (number): Grid width.
        start_pos (table): {y, x} coordinates of the starting position.
        target_steps (number): The exact number of steps to take.
        infinite (boolean): True if the grid repeats infinitely.
]]
local function count_reachable(grid, height, width, start_pos, target_steps, infinite)
    -- Set of currently reachable positions, keys are "y,x", value is true
    local current_positions = {}
    current_positions[start_pos.y .. "," .. start_pos.x] = true

    -- Store counts at specific steps needed for quadratic fitting in Part 2
    local counts_at_steps = {}

    -- Neighbor offsets: North, South, East, West
    local deltas = { { -1, 0 }, { 1, 0 }, { 0, -1 }, { 0, 1 } }

    for step = 1, target_steps do
        local next_positions = {}

        for pos_key, _ in pairs(current_positions) do
            local current_y, current_x = parse_key(pos_key)

            for _, d in ipairs(deltas) do
                local next_y, next_x = current_y + d[1], current_x + d[2]
                local next_key = next_y .. "," .. next_x

                local is_valid_move = false
                if infinite then
                    -- Infinite grid: use modulo arithmetic to wrap coordinates
                    -- Lua uses 1-based indexing, adjust modulo carefully
                    local map_y = (next_y - 1) % height + 1
                    local map_x = (next_x - 1) % width + 1
                    if grid[map_y][map_x] ~= '#' then
                        is_valid_move = true
                    end
                else
                    -- Finite grid: check bounds and obstacles
                    if next_y >= 1 and next_y <= height and
                       next_x >= 1 and next_x <= width and
                       grid[next_y][next_x] ~= '#' then
                        is_valid_move = true
                    end
                end

                if is_valid_move then
                    -- Add to the set for the next step. Duplicates are automatically handled by table keys.
                    next_positions[next_key] = true
                end
            end
        end
        current_positions = next_positions -- Move to the next layer of reachable positions

        -- Store counts if needed for Part 2 analysis
        if infinite then
             -- Check if current step matches required steps for polynomial fitting
             local offset = math.floor(width / 2) -- Assuming square grid
             if step == offset or step == offset + width or step == offset + 2 * width then
                 local count = 0
                 for _ in pairs(current_positions) do count = count + 1 end
                 counts_at_steps[step] = count
             end
        end

    end

    -- Calculate the final count after target_steps
    local final_count = 0
    for _ in pairs(current_positions) do
        final_count = final_count + 1
    end

    -- If infinite mode, return counts needed for fitting AND final count (if target_steps was small)
    if infinite then
        return final_count, counts_at_steps
    else
        return final_count -- Return only the final count for Part 1
    end
end

-- Main function to orchestrate reading input and solving parts
local function solve()
    -- 1. Read Input
    local lines = {}
    local file = io.open("input.txt", "r")
    if not file then
        error("Could not open input.txt")
        return
    end
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    -- 2. Parse Grid
    local height = #lines
    local width = #lines[1]
    local grid = {}
    local start_pos = nil

    for y = 1, height do
        grid[y] = {} -- Initialize row
        for x = 1, width do
            local char = string.sub(lines[y], x, x)
            grid[y][x] = char
            if char == 'S' then
                if start_pos then error("Multiple start positions 'S' found.") end
                start_pos = { y = y, x = x }
                grid[y][x] = '.' -- Treat 'S' as a garden plot '.'
            end
        end
    end

    if not start_pos then error("Start position 'S' not found.") end

    -- Basic Grid Sanity Checks (useful for Part 2 assumptions)
    if width ~= height then
        print("Warning: Grid is not square. Part 2 logic assumes a square grid.")
    end
    if width % 2 == 0 then
         print("Warning: Grid size is even. Part 2 logic assumes odd size for center start.")
    end
    local expected_center = math.floor(width / 2) + 1
    if start_pos.x ~= expected_center or start_pos.y ~= expected_center then
         print("Warning: Start position 'S' is not in the exact center. Part 2 logic assumes center start.")
    end


    -- 3. Solve Part 1
    local steps1 = 64
    print("Calculating Part 1...")
    local part1_result = count_reachable(grid, height, width, start_pos, steps1, false)
    print("Part 1:", part1_result)

    -- 4. Solve Part 2
    local steps2 = 26501365

    -- Part 2 requires observations about the infinite grid growth.
    -- The number of reachable plots grows quadratically for large steps.
    -- We can find this quadratic function f(x) = ax^2 + bx + c by finding the
    -- number of plots reachable at 3 specific step counts related to the grid size.
    -- Let N be the grid size (width/height). The steps are typically N/2, N/2 + N, N/2 + 2N.
    -- Here, x represents the number of grid repetitions outwards (0, 1, 2).

    local size = width -- Assuming square grid for simplicity
    local offset = math.floor(size / 2) -- Steps to reach the edge from the center

    -- Required step counts for quadratic fitting
    local s0 = offset
    local s1 = offset + size
    local s2 = offset + 2 * size

    print("\nCalculating points for Part 2 quadratic fit...")
    -- Run BFS once up to the largest needed step count (s2) in infinite mode
    -- The function will return the counts captured at s0, s1, s2
    local _, captured_counts = count_reachable(grid, height, width, start_pos, s2, true)

    local y0 = captured_counts[s0]
    local y1 = captured_counts[s1]
    local y2 = captured_counts[s2]

    if not y0 or not y1 or not y2 then
        error("Failed to capture counts needed for quadratic fitting at steps: " .. s0 .. ", " .. s1 .. ", " .. s2)
        return
    end

    print("Counts at steps " .. s0 .. ", " .. s1 .. ", " .. s2 .. ":", y0, y1, y2)

    -- Calculate coefficients a, b, c for f(x) = ax^2 + bx + c
    -- Using Lagrange interpolation or solving the system:
    -- y0 = a*0^2 + b*0 + c  => c = y0
    -- y1 = a*1^2 + b*1 + c  => y1 = a + b + c
    -- y2 = a*2^2 + b*2 + c  => y2 = 4a + 2b + c

    local c = y0
    -- From y1: a + b = y1 - c = y1 - y0
    -- From y2: 4a + 2b = y2 - c = y2 - y0
    -- Multiply (a+b) by 2: 2a + 2b = 2*(y1 - y0)
    -- Subtract this from (4a + 2b): (4a + 2b) - (2a + 2b) = (y2 - y0) - 2*(y1 - y0)
    -- 2a = y2 - y0 - 2*y1 + 2*y0
    -- 2a = y2 - 2*y1 + y0
    local a = (y2 - 2 * y1 + y0) / 2

    -- b = (y1 - y0) - a
    local b = (y1 - y0) - a

    -- Sanity check: coefficients should be integers for this problem type
    if a ~= math.floor(a) or b ~= math.floor(b) or c ~= math.floor(c) then
        print("Warning: Quadratic coefficients are not integers. Result might be inaccurate.", a, b, c)
    end
    print("Quadratic coefficients (a, b, c):", a, b, c)

    -- Now evaluate the quadratic for the target steps
    -- Target steps = k * size + offset
    -- We need to find k (number of grid repetitions)
    if (steps2 - offset) % size ~= 0 then
        print("Warning: Target steps " .. steps2 .. " does not align perfectly with grid size " .. size .. " and offset " .. offset .. ". Formula might be approximate.")
    end
    local k = math.floor((steps2 - offset) / size) -- Integer division

    print("Target grid repetitions (k):", k)

    -- Calculate final result using the quadratic: a*k^2 + b*k + c
    -- Use tonumber to potentially force higher precision if needed, though Lua numbers often handle large integers.
    local part2_result = tonumber(a) * tonumber(k) * tonumber(k) + tonumber(b) * tonumber(k) + tonumber(c)

    -- Format output for potentially very large numbers
    print("Part 2:", string.format("%.0f", part2_result))

end

-- Proper main entry point
local function main()
    local status, err = pcall(solve)
    if not status then
        print("An error occurred:")
        print(err)
    end
end

-- Execute the main function
main()
