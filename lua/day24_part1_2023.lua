function parseInput(input)
    local points = {}
    for i, line in ipairs(input) do
        local x, y, z, vx, vy, vz = line:match("^(%-?%d+), (%-?%d+), (%-?%d+) @ (%-?%d+), (%-?%d+), (%-?%d+)$")
        if x and y and z and vx and vy and vz then
            local point = {
                pos = {x = tonumber(x), y = tonumber(y), z = tonumber(z)},
                vel = {x = tonumber(vx), y = tonumber(vy), z = tonumber(vz)}
            }
            points[i] = point
        else
            print("Failed to parse line: " .. line)
        end
    end
    return points
end

function isIntersecting2D(p1, p2)
    local det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y
    if det == 0 then
        return false, {}, 0, 0
    end
    local t1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det
    local t2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det
    local coord = {
        x = p1.pos.x + p1.vel.x * t1,
        y = p1.pos.y + p1.vel.y * t1,
        z = 0
    }
    return true, coord, t1, t2
end

function solve(input, min, max)
    local points = parseInput(input)
    local cnt = 0
    for i = 1, #points do
        for j = 1, i - 1 do
            local isIntersecting, coord, time1, time2 = isIntersecting2D(points[i], points[j])
            if isIntersecting then
                local isInBound = min <= coord.x and coord.x <= max and min <= coord.y and coord.y <= max
                if isInBound and time1 >= 0 and time2 >= 0 then
                    cnt = cnt + 1
                end
            end
        end
    end
    return cnt
end

function readFile(fileName)
    local file = io.open(fileName, "r")
    local content = file:read("*all")
    file:close()
    return content:split("\n")
end

function string:split(delimiter)
    local result = {}
    local from = 1
    local delim_from, delim_to = self:find(delimiter, from)
    while delim_from do
        table.insert(result, self:sub(from, delim_from - 1))
        from = delim_to + 1
        delim_from, delim_to = self:find(delimiter, from)
    end
    table.insert(result, self:sub(from))
    return result
end

local input = readFile("input.txt")
print(solve(input, 200000000000000, 400000000000000))