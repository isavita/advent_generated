local md5 = require("md5")

-- Read the passcode from input.txt
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then
        error("Could not open input.txt")
    end
    local passcode = file:read("*l")
    file:close()
    return passcode
end

-- Generate MD5 hash and get the first 4 characters
local function get_doors(passcode, path)
    local hash = md5.sumhexa(passcode .. path)
    return hash:sub(1, 4)
end

-- Check if a door is open
local function is_open(char)
    return char:match("[b-f]") ~= nil
end

-- Check if position is valid
local function is_valid(x, y)
    return x >= 1 and x <= 4 and y >= 1 and y <= 4
end

-- Breadth-First Search to find the shortest path
local function find_shortest_path(passcode)
    local queue = {{x = 1, y = 1, path = ""}}
    local directions = {
        {dx = 0, dy = -1, dir = "U"},
        {dx = 0, dy = 1, dir = "D"},
        {dx = -1, dy = 0, dir = "L"},
        {dx = 1, dy = 0, dir = "R"}
    }

    while #queue > 0 do
        local current = table.remove(queue, 1)
        if current.x == 4 and current.y == 4 then
            return current.path
        end

        local doors = get_doors(passcode, current.path)
        for i, dir in ipairs(directions) do
            if is_open(doors:sub(i, i)) then
                local new_x, new_y = current.x + dir.dx, current.y + dir.dy
                if is_valid(new_x, new_y) then
                    table.insert(queue, {
                        x = new_x,
                        y = new_y,
                        path = current.path .. dir.dir
                    })
                end
            end
        end
    end

    return nil  -- No path found
end

-- Main function to solve the problem
local function solve()
    local passcode = read_input()
    return find_shortest_path(passcode)
end

-- Run the solver and print the result
local result = solve()
print(result or "No path found")
