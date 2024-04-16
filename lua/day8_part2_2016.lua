local screenWidth = 50
local screenHeight = 6

local function createScreen()
    local screen = {}
    for y = 1, screenHeight do
        screen[y] = {}
        for x = 1, screenWidth do
            screen[y][x] = false
        end
    end
    return screen
end

local function displayScreen(screen)
    for y = 1, screenHeight do
        local line = ""
        for x = 1, screenWidth do
            if screen[y][x] then
                line = line .. "#"
            else
                line = line .. "."
            end
        end
        print(line)
    end
end

local function rect(screen, a, b)
    for y = 1, b do
        for x = 1, a do
            screen[y][x] = true
        end
    end
end

local function rotateRow(screen, row, shift)
    local temp = {}
    for x = 1, screenWidth do
        temp[(x + shift - 1) % screenWidth + 1] = screen[row][x]
    end
    for x = 1, screenWidth do
        screen[row][x] = temp[x]
    end
end

local function rotateColumn(screen, col, shift)
    local temp = {}
    for y = 1, screenHeight do
        temp[(y + shift - 1) % screenHeight + 1] = screen[y][col]
    end
    for y = 1, screenHeight do
        screen[y][col] = temp[y]
    end
end

local function processInstruction(instruction, screen)
    local a, b = instruction:match("rect (%d+)x(%d+)")
    local row, shiftRow = instruction:match("rotate row y=(%d+) by (%d+)")
    local col, shiftCol = instruction:match("rotate column x=(%d+) by (%d+)")

    if a and b then
        rect(screen, tonumber(a), tonumber(b))
    elseif row and shiftRow then
        rotateRow(screen, tonumber(row) + 1, tonumber(shiftRow))
    elseif col and shiftCol then
        rotateColumn(screen, tonumber(col) + 1, tonumber(shiftCol))
    end
end

local function main()
    local screen = createScreen()
    local file = io.open("input.txt", "r")
    if not file then
        error("Failed to open file")
    end

    for line in file:lines() do
        processInstruction(line, screen)
    end

    file:close()
    displayScreen(screen)
end

main()