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

-- Depth-First Search to find the longest path
local function find_longest_path(passcode)
    local longest_path_length = 0
    local directions = {
        {dx = 0, dy = -1, dir = "U"},
        {dx = 0, dy = 1, dir = "D"},
        {dx = -1, dy = 0, dir = "L"},
        {dx = 1, dy = 0, dir = "R"}
    }

    local function dfs(x, y, path)
        if x == 4 and y == 4 then
            longest_path_length = math.max(longest_path_length, #path)
            return
        end

        local doors = get_doors(passcode, path)
        for i, dir in ipairs(directions) do
            if is_open(doors:sub(i, i)) then
                local new_x, new_y = x + dir.dx, y + dir.dy
                if is_valid(new_x, new_y) then
                    dfs(new_x, new_y, path .. dir.dir)
                end
            end
        end
    end

    dfs(1, 1, "")
    return longest_path_length
end

-- Main function to solve the problem
local function solve()
    local passcode = read_input()
    return find_longest_path(passcode)
end

-- Run the solver and print the result
local result = solve()
print(result)
