-- Function to compute the modular inverse of a number a modulo m
-- Using Extended Euclidean Algorithm
local function mod_inverse(a, m)
    local m0, x0, x1 = m, 0, 1
    if m == 1 then return 0 end
    while a > 1 do
        local q = math.floor(a / m)
        local t = m
        m = a % m
        a = t
        t = x0
        x0 = x1 - q * x0
        x1 = t
    end
    if x1 < 0 then x1 = x1 + m0 end
    return x1
end

-- Function to solve the Chinese Remainder Theorem problem
-- Returns the smallest non-negative solution to the system of congruences
local function chinese_remainder_theorem(bus_ids)
    local timestamp, step = 0, 1

    for i, id in ipairs(bus_ids) do
        if id == 'x' then
            -- Skip 'x' entries
            goto continue
        end

        local bus_id = tonumber(id)
        local offset = i - 1

        -- Solve for the current bus_id
        while (timestamp + offset) % bus_id ~= 0 do
            timestamp = timestamp + step
        end

        -- Update step to be the least common multiple of the current step and bus_id
        step = step * bus_id

        ::continue::
    end

    return timestamp
end

-- Main function to read input and compute the result
local function main()
    -- Read input from file
    local file = io.open("input.txt", "r")
    local lines = file:read("*a")
    file:close()

    -- Split into lines
    local line1, line2 = lines:match("([^\n]+)\n([^\n]+)")

    -- Split the second line into bus IDs
    local bus_ids = {}
    for id in line2:gmatch("[^,]+") do
        table.insert(bus_ids, id)
    end

    -- Compute and print the earliest timestamp
    local result = chinese_remainder_theorem(bus_ids)
    print(result)
end

-- Run the main function
main()
