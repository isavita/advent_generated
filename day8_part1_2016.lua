
local screenWidth = 50
local screenHeight = 6

local function rect(screen, a, b)
    for y = 1, b do
        for x = 1, a do
            screen[y][x] = true
        end
    end
end

local function rotateRow(screen, row, shift)
    local temp = {}
    for i = 1, screenWidth do
        temp[(i + shift - 1) % screenWidth + 1] = screen[row][i]
    end
    screen[row] = temp
end

local function rotateColumn(screen, col, shift)
    local temp = {}
    for i = 1, screenHeight do
        temp[(i + shift - 1) % screenHeight + 1] = screen[i][col]
    end
    for i = 1, screenHeight do
        screen[i][col] = temp[i]
    end
end

local function countLitPixels(screen)
    local count = 0
    for _, row in ipairs(screen) do
        for _, pixel in ipairs(row) do
            if pixel then
                count = count + 1
            end
        end
    end
    return count
end

local file = io.open("input.txt", "r")
local screen = {}
for i = 1, screenHeight do
    screen[i] = {}
    for j = 1, screenWidth do
        screen[i][j] = false
    end
end

for line in file:lines() do
    if line:match("rect (%d+)x(%d+)") then
        local a, b = line:match("rect (%d+)x(%d+)")
        rect(screen, tonumber(a), tonumber(b))
    elseif line:match("rotate row y=(%d+) by (%d+)") then
        local row, shift = line:match("rotate row y=(%d+) by (%d+)")
        rotateRow(screen, tonumber(row) + 1, tonumber(shift))
    elseif line:match("rotate column x=(%d+) by (%d+)") then
        local col, shift = line:match("rotate column x=(%d+) by (%d+)")
        rotateColumn(screen, tonumber(col) + 1, tonumber(shift))
    end
end

print(countLitPixels(screen))

file:close()
