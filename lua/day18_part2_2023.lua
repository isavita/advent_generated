
local function parseInput(input)
    local North = {0, -1}
    local West = {-1, 0}
    local South = {0, 1}
    local East = {1, 0}

    local function hexStringToInt(hexStr)
        return tonumber(hexStr, 16)
    end

    local function Abs(x)
        if x < 0 then
            return -x
        end
        return x
    end

    local function shoelace(vertices)
        local n = #vertices
        local area = 0

        for i = 1, n do
            local next = (i % n) + 1
            area = area + vertices[i][1] * vertices[next][2]
            area = area - vertices[i][2] * vertices[next][1]
        end

        area = Abs(area) / 2
        return area
    end

    local function perimeter(vertices)
        local n = #vertices
        local perim = 0

        for i = 1, n do
            local next = (i % n) + 1
            perim = perim + Abs(vertices[i][1] - vertices[next][1]) + Abs(vertices[i][2] - vertices[next][2])
        end

        return perim
    end

    local function calculatePolygonArea(vertices)
        return shoelace(vertices) + perimeter(vertices) / 2 + 1
    end

    local current = {0, 0}
    local vertices = {current}

    for i = 1, #input do
        local parts = {}
        for part in string.gmatch(input[i], "%S+") do
            table.insert(parts, part)
        end
        local color = parts[3]
        local dirInput = string.byte(color, 8)
        local lengthStr = string.sub(color, 3, 7)
        local length = hexStringToInt(lengthStr)

        local dir
        if dirInput == 51 then
            dir = North
        elseif dirInput == 50 then
            dir = West
        elseif dirInput == 49 then
            dir = South
        elseif dirInput == 48 then
            dir = East
        end

        current = {current[1] + dir[1] * length, current[2] + dir[2] * length}
        table.insert(vertices, current)
    end

    return calculatePolygonArea(vertices)
end

local function readFile(fileName)
    local file = io.open(fileName, "r")
    local content = file:read("*all")
    file:close()
    return content
end

local input = {}
for line in io.lines("input.txt") do
    table.insert(input, line)
end

print(parseInput(input))
