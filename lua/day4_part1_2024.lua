
local grid = {}
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

-- Read all non-empty lines into grid
local line
while true do
    line = file:read()
    if not line then break end
    line = line:gsub("%s+", "") -- Remove all whitespace
    if line ~= "" then
        table.insert(grid, line)
    end
end
file:close()

-- Define all possible directions
local directions = {
    {dx = 0, dy = 1},   -- Right
    {dx = 1, dy = 0},   -- Down
    {dx = 1, dy = 1},   -- Down-right
    {dx = -1, dy = 1},  -- Up-right
    {dx = 0, dy = -1},  -- Left
    {dx = -1, dy = 0},  -- Up
    {dx = -1, dy = -1}, -- Up-left
    {dx = 1, dy = -1},  -- Down-left
}

-- Precompute word characters for faster access
local word = "XMAS"
local word_chars = {}
for i = 1, #word do
    word_chars[i] = word:sub(i, i)
end

local function checkWord(x, y, dx, dy)
    for i = 0, 3 do
        local new_x = x + dx * i
        local new_y = y + dy * i
        -- Check if new position is within grid bounds
        if new_x < 1 or new_x > #grid then
            return false
        end
        local row = grid[new_x]
        if new_y < 1 or new_y > #row then
            return false
        end
        -- Compare characters
        if row:sub(new_y, new_y) ~= word_chars[i + 1] then
            return false
        end
    end
    return true
end

local count = 0

-- Iterate through each cell in the grid
for i = 1, #grid do
    local row = grid[i]
    for j = 1, #row do
        -- Skip if starting character isn't 'X'
        if row:sub(j, j) ~= 'X' then
            goto continue
        end
        -- Check all directions
        for _, dir in ipairs(directions) do
            if checkWord(i, j, dir.dx, dir.dy) then
                count = count + 1
            end
        end
        ::continue::
    end
end

print(string.format("XMAS appears %d times in the word search", count))
