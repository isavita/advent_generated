-- Define the split function for strings before it's used
function string:split(delimiter)
    local result = {}
    local from = 1
    local delim_from, delim_to = string.find(self, delimiter, from)
    while delim_from do
        table.insert(result, string.sub(self, from, delim_from - 1))
        from = delim_to + 1
        delim_from, delim_to = string.find(self, delimiter, from)
    end
    table.insert(result, string.sub(self, from))
    return result
end

function firstRevisitedDistance(instructions)
    local pos = {x = 0, y = 0}
    local visited = {}
    visited[pos.x .. "," .. pos.y] = true
    local directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}
    local dirIndex = 1 -- Start facing North

    for _, instruction in ipairs(instructions) do
        local turn = instruction:sub(1, 1)
        local blocks = tonumber(instruction:sub(2))

        if turn == "R" then
            dirIndex = (dirIndex % 4) + 1
        else
            dirIndex = (dirIndex + 2) % 4 + 1
        end

        for i = 1, blocks do
            pos.x = pos.x + directions[dirIndex][1]
            pos.y = pos.y + directions[dirIndex][2]

            local key = pos.x .. "," .. pos.y
            if visited[key] then
                return math.abs(pos.x) + math.abs(pos.y)
            end
            visited[key] = true
        end
    end

    return -1 -- No location visited twice
end

-- Read instructions from file
local file = io.open("input.txt", "r")
local instructions
if file then
    instructions = file:read("*line"):split(", ")
    file:close()
end

-- Output the result of the function
if instructions then
    print(firstRevisitedDistance(instructions))
else
    print("No instructions found.")
end