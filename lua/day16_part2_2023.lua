local function read_input()
    local grid = {}
    for line in io.lines("input.txt") do
        table.insert(grid, line)
    end
    return grid
end

local function is_valid(x, y, width, height)
    return x >= 1 and x <= width and y >= 1 and y <= height
end

local function trace_beam(grid, start_x, start_y, start_dx, start_dy)
    local width, height = #grid[1], #grid
    local energized = {}
    local visited = {}
    local queue = {{start_x, start_y, start_dx, start_dy}}

    while #queue > 0 do
        local x, y, dx, dy = table.unpack(table.remove(queue, 1))
        
        if not is_valid(x, y, width, height) then
            goto continue
        end

        energized[y] = energized[y] or {}
        energized[y][x] = true

        local key = string.format("%d,%d,%d,%d", x, y, dx, dy)
        if visited[key] then
            goto continue
        end
        visited[key] = true

        local char = grid[y]:sub(x, x)
        if char == '.' or (char == '-' and dy == 0) or (char == '|' and dx == 0) then
            table.insert(queue, {x + dx, y + dy, dx, dy})
        elseif char == '/' then
            table.insert(queue, {x - dy, y - dx, -dy, -dx})
        elseif char == '\\' then
            table.insert(queue, {x + dy, y + dx, dy, dx})
        elseif char == '|' then
            table.insert(queue, {x, y - 1, 0, -1})
            table.insert(queue, {x, y + 1, 0, 1})
        elseif char == '-' then
            table.insert(queue, {x - 1, y, -1, 0})
            table.insert(queue, {x + 1, y, 1, 0})
        end

        ::continue::
    end

    local count = 0
    for _, row in pairs(energized) do
        for _ in pairs(row) do
            count = count + 1
        end
    end
    return count
end

local grid = read_input()
local width, height = #grid[1], #grid
local max_energized = 0

-- Top and bottom rows
for x = 1, width do
    max_energized = math.max(max_energized, trace_beam(grid, x, 1, 0, 1))
    max_energized = math.max(max_energized, trace_beam(grid, x, height, 0, -1))
end

-- Left and right columns
for y = 1, height do
    max_energized = math.max(max_energized, trace_beam(grid, 1, y, 1, 0))
    max_energized = math.max(max_energized, trace_beam(grid, width, y, -1, 0))
end

print(max_energized)
