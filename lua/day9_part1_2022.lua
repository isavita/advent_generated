-- Function to read the directions and steps from a file
local function readDirectionsFromFile(filename)
    local directions = {}
    local file = io.open(filename, "r")
    if file then
        for line in file:lines() do
            local dir, steps = line:match("(%a)%s+(%d+)")
            table.insert(directions, {dir = dir, steps = tonumber(steps)})
        end
        file:close()
    end
    return directions
end

local function main()
    local head = {x = 0, y = 0}
    local tail = {x = 0, y = 0}
    local visited = {}
    visited[tail.x .. "," .. tail.y] = true

    local directions = readDirectionsFromFile("input.txt")

    for _, direction in ipairs(directions) do
        local dir = direction.dir
        local num_steps = direction.steps

        for _ = 1, num_steps do
            if dir == "R" then
                head.x = head.x + 1
            elseif dir == "L" then
                head.x = head.x - 1
            elseif dir == "U" then
                head.y = head.y + 1
            elseif dir == "D" then
                head.y = head.y - 1
            end

            while math.abs(head.x - tail.x) > 1 or math.abs(head.y - tail.y) > 1 do
                if head.x ~= tail.x then
                    tail.x = tail.x + (head.x > tail.x and 1 or -1)
                end
                if head.y ~= tail.y then
                    tail.y = tail.y + (head.y > tail.y and 1 or -1)
                end
                visited[tail.x .. "," .. tail.y] = true
            end
        end
    end

    print("Total visited positions: " .. table.count(visited))
end

-- Helper function to count the number of keys in a table
function table.count(t)
    local count = 0
    for _ in pairs(t) do count = count + 1 end
    return count
end

main()