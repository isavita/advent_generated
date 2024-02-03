
function readAll()
    local file = io.open("input.txt", "r")
    local content = file:read("*a")
    file:close()
    return content
end

function abs(x)
    if x < 0 then
        return -x
    end
    return x
end

function sign(n)
    if n == 0 then
        return 0
    end
    if n < 0 then
        return -1
    end
    return 1
end

function next(head, tail)
    if abs(head.x - tail.x) <= 1 and abs(head.y - tail.y) <= 1 then
        return tail
    end
    return {x = tail.x + sign(head.x - tail.x), y = tail.y + sign(head.y - tail.y)}
end

function visited(input, ropelen)
    local rope = {}
    for i = 1, ropelen do
        rope[i] = {x = 0, y = 0}
    end
    local visited = {}
    for line in string.gmatch(input, "[^\n]+") do
        local b, n = string.match(line, "(%a) (%d+)")
        local d = dirFromByte(b)
        for i = 1, tonumber(n) do
            rope[1].x = rope[1].x + d.x
            rope[1].y = rope[1].y + d.y
            for j = 2, ropelen do
                rope[j] = next(rope[j - 1], rope[j])
            end
            visited[rope[ropelen].x .. "," .. rope[ropelen].y] = true
        end
    end
    local count = 0
    for k, v in pairs(visited) do
        count = count + 1
    end
    return count
end

function dirFromByte(b)
    local fromByte = {
        ["N"] = {x = 0, y = 1},
        ["E"] = {x = 1, y = 0},
        ["S"] = {x = 0, y = -1},
        ["W"] = {x = -1, y = 0},
        ["U"] = {x = 0, y = 1},
        ["R"] = {x = 1, y = 0},
        ["D"] = {x = 0, y = -1},
        ["L"] = {x = -1, y = 0},
        ["^"] = {x = 0, y = 1},
        [">"] = {x = 1, y = 0},
        ["v"] = {x = 0, y = -1},
        ["<"] = {x = -1, y = 0}
    }
    return fromByte[b]
end

local input = readAll()
print(visited(input, 10))
