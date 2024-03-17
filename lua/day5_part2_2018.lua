-- Read input from file
local file = io.open("input.txt", "r")
local polymer = file:read("*a")
file:close()

-- Remove whitespace
polymer = polymer:gsub("%s+", "")

-- Reaction function
local function react(p)
    local reactionOccurred = true
    while reactionOccurred do
        reactionOccurred = false
        for i = 1, #p - 1 do
            if p:sub(i, i) ~= p:sub(i + 1, i + 1) and p:sub(i, i):upper() == p:sub(i + 1, i + 1):upper() then
                p = p:sub(1, i - 1) .. p:sub(i + 2)
                reactionOccurred = true
            end
        end
    end
    return p
end

-- Find the minimum length
local minLength = #polymer
for i = 1, 26 do
    local unit = string.char(96 + i)
    local tempPolymer = polymer:gsub(unit, ""):gsub(unit:upper(), "")
    local reactedPolymer = react(tempPolymer)
    if #reactedPolymer < minLength then
        minLength = #reactedPolymer
    end
end

-- Print the answer
print(minLength)