-- Define the function to count entries in a table
function table.count(t)
    local count = 0
    for _ in pairs(t) do
        count = count + 1
    end
    return count
end

-- Open the file
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file.")
    return
end

-- Initialize data structures
local points = {}
local folds = {}
local readingPoints = true

-- Read the file line by line
for line in file:lines() do
    if line == "" then
        readingPoints = false
    elseif readingPoints then
        local x, y = line:match("^(%d+),(%d+)$")
        x = tonumber(x)
        y = tonumber(y)
        points[x .. "," .. y] = true
    else
        table.insert(folds, line)
    end
end

-- Close the file
file:close()

-- Process the first fold instruction
local fold = folds[1]
local axis, value = fold:match("fold along (%a)=(%d+)")
value = tonumber(value)

-- Create a new table for the folded points
local newPoints = {}
if axis == "x" then
    for key, _ in pairs(points) do
        local x, y = key:match("^(%d+),(%d+)$")
        x = tonumber(x)
        y = tonumber(y)
        if x > value then
            x = 2 * value - x
        end
        newPoints[x .. "," .. y] = true
    end
else
    for key, _ in pairs(points) do
        local x, y = key:match("^(%d+),(%d+)$")
        x = tonumber(x)
        y = tonumber(y)
        if y > value then
            y = 2 * value - y
        end
        newPoints[x .. "," .. y] = true
    end
end

-- Print the count of unique points after the fold
print(table.count(newPoints))