-- Function to sort characters in a string
local function sortString(str)
    local chars = {}
    for c in str:gmatch(".") do
        table.insert(chars, c)
    end
    table.sort(chars)
    return table.concat(chars)
end

-- Function to find the difference between two strings
local function stringDiff(str1, str2)
    local diff = ""
    for c in str1:gmatch(".") do
        if not str2:find(c) then
            diff = diff .. c
        end
    end
    return diff
end

-- Function to decode the patterns
local function decodePatterns(patterns)
    local digits = {}
    local fiveSegments = {}
    local sixSegments = {}

    for _, pattern in ipairs(patterns) do
        local len = #pattern
        if len == 2 then digits[1] = pattern
        elseif len == 3 then digits[7] = pattern
        elseif len == 4 then digits[4] = pattern
        elseif len == 7 then digits[8] = pattern
        elseif len == 5 then table.insert(fiveSegments, pattern)
        elseif len == 6 then table.insert(sixSegments, pattern)
        end
    end

    -- Find 3
    for _, pattern in ipairs(fiveSegments) do
        if #stringDiff(digits[1], pattern) == 0 then
            digits[3] = pattern
            break
        end
    end

    -- Find 6
    for _, pattern in ipairs(sixSegments) do
        if #stringDiff(digits[1], pattern) == 1 then
            digits[6] = pattern
            break
        end
    end

    -- Find 5 and 2
    for _, pattern in ipairs(fiveSegments) do
        if pattern ~= digits[3] then
            if #stringDiff(pattern, digits[6]) == 0 then
                digits[5] = pattern
            else
                digits[2] = pattern
            end
        end
    end

    -- Find 0 and 9
    for _, pattern in ipairs(sixSegments) do
        if pattern ~= digits[6] then
            if #stringDiff(digits[3], pattern) == 0 then
                digits[9] = pattern
            else
                digits[0] = pattern
            end
        end
    end

    -- Create a lookup table
    local lookup = {}
    for digit, pattern in pairs(digits) do
        lookup[sortString(pattern)] = digit
    end

    return lookup
end

-- Main function
local function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error: Could not open input file")
        return
    end

    local total = 0

    for line in file:lines() do
        local patterns, output = line:match("(.+) | (.+)")
        local patternList = {}
        for pattern in patterns:gmatch("%S+") do
            table.insert(patternList, pattern)
        end

        local lookup = decodePatterns(patternList)
        
        local value = 0
        for digit in output:gmatch("%S+") do
            value = value * 10 + lookup[sortString(digit)]
        end
        
        total = total + value
    end

    file:close()
    print(total)
end

main()
