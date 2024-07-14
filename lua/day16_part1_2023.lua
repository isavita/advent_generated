-- Function to read the input file
local function readInput()
    local grid = {}
    for line in io.lines("input.txt") do
        table.insert(grid, line)
    end
    return grid
end

-- Function to move the beam
local function moveBeam(grid, x, y, dx, dy, energized, beams)
    local key = x .. "," .. y .. "," .. dx .. "," .. dy
    if beams[key] then return end
    beams[key] = true

    while x >= 1 and x <= #grid[1] and y >= 1 and y <= #grid do
        energized[y .. "," .. x] = true
        local char = grid[y]:sub(x, x)

        if char == "/" then
            dx, dy = -dy, -dx
        elseif char == "\\" then
            dx, dy = dy, dx
        elseif char == "|" and dx ~= 0 then
            moveBeam(grid, x, y-1, 0, -1, energized, beams)
            moveBeam(grid, x, y+1, 0, 1, energized, beams)
            return
        elseif char == "-" and dy ~= 0 then
            moveBeam(grid, x-1, y, -1, 0, energized, beams)
            moveBeam(grid, x+1, y, 1, 0, energized, beams)
            return
        end

        x, y = x + dx, y + dy
    end
end

-- Main function
local function main()
    local grid = readInput()
    local energized = {}
    local beams = {}

    moveBeam(grid, 1, 1, 1, 0, energized, beams)

    local count = 0
    for _ in pairs(energized) do
        count = count + 1
    end

    print(count)
end

main()
