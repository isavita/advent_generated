-- Function to check if two strings are equal
local function areEqual(str1, str2)
    return str1 == str2
end

-- Function to check if a pattern has a horizontal reflection at a given row
local function checkHorizontalReflection(pattern, row)
    local up = row
    local down = row + 1
    while up > 0 and down <= #pattern do
        if not areEqual(pattern[up], pattern[down]) then
            return false
        end
        up = up - 1
        down = down + 1
    end
    return true
end

-- Function to check if a pattern has a vertical reflection at a given column
local function checkVerticalReflection(pattern, col)
    local left = col
    local right = col + 1
    while left > 0 and right <= #pattern[1] do
        for i = 1, #pattern do
            if pattern[i]:sub(left, left) ~= pattern[i]:sub(right, right) then
                return false
            end
        end
        left = left - 1
        right = right + 1
    end
    return true
end

-- Function to find the reflection line and calculate the score
local function findReflection(pattern)
    -- Check for horizontal reflection
    for i = 1, #pattern - 1 do
        if checkHorizontalReflection(pattern, i) then
            return i * 100
        end
    end
    
    -- Check for vertical reflection
    for i = 1, #pattern[1] - 1 do
        if checkVerticalReflection(pattern, i) then
            return i
        end
    end
    
    return 0  -- No reflection found
end

-- Read input from file
local file = io.open("input.txt", "r")
if not file then
    print("Error: Could not open input file")
    os.exit(1)
end

local patterns = {}
local currentPattern = {}

for line in file:lines() do
    if line ~= "" then
        table.insert(currentPattern, line)
    else
        if #currentPattern > 0 then
            table.insert(patterns, currentPattern)
            currentPattern = {}
        end
    end
end

if #currentPattern > 0 then
    table.insert(patterns, currentPattern)
end

file:close()

-- Process patterns and calculate total score
local totalScore = 0
for _, pattern in ipairs(patterns) do
    totalScore = totalScore + findReflection(pattern)
end

-- Print the result
print(totalScore)
