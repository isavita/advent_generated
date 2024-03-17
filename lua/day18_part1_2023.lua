local function abs(x)
    return x < 0 and -x or x
end

local function shoelace(vertices)
    local n = #vertices
    local area = 0
    for i = 1, n do
        local next = (i % n) + 1
        area = area + vertices[i].x * vertices[next].y
        area = area - vertices[i].y * vertices[next].x
    end
    return abs(area) / 2
end

local function perimeter(vertices)
    local n = #vertices
    local perim = 0
    for i = 1, n do
        local next = (i % n) + 1
        perim = perim + abs(vertices[i].x - vertices[next].x) + abs(vertices[i].y - vertices[next].y)
    end
    return perim
end

local function calculate_polygon_area(vertices)
    return shoelace(vertices) + perimeter(vertices) / 2 + 1
end

local function parse_input(input)
    local current = {x = 0, y = 0}
    local vertices = {current}
    local directions = {U = {0, -1}, L = {-1, 0}, D = {0, 1}, R = {1, 0}}
    for _, line in ipairs(input) do
        local parts = {}
        for part in line:gmatch("%S+") do
            table.insert(parts, part)
        end
        local dir = directions[parts[1]:sub(1, 1)]
        local length = tonumber(parts[2])
        current = {x = current.x + dir[1] * length, y = current.y + dir[2] * length}
        table.insert(vertices, current)
    end
    return vertices
end

local function solve(input)
    local vertices = parse_input(input)
    return calculate_polygon_area(vertices)
end

local function read_file(file_name)
    local file = io.open(file_name, "r")
    if not file then
        error("Failed to open file: " .. file_name)
    end
    local input = {}
    for line in file:lines() do
        table.insert(input, line)
    end
    file:close()
    return input
end

local input = read_file("input.txt")
print(solve(input))