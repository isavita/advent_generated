
local file = io.open("input.txt", "r")
local directions = {
    e = {1, 0},
    se = {0, 1},
    sw = {-1, 1},
    w = {-1, 0},
    nw = {0, -1},
    ne = {1, -1}
}

local blackTiles = {}

for line in file:lines() do
    local coord = {0, 0}
    local i = 1

    while i <= #line do
        local dir
        if line:sub(i, i) == 'e' or line:sub(i, i) == 'w' then
            dir = line:sub(i, i)
        else
            dir = line:sub(i, i + 1)
            i = i + 1
        end

        local move = directions[dir]
        coord[1] = coord[1] + move[1]
        coord[2] = coord[2] + move[2]

        i = i + 1
    end

    blackTiles[tostring(coord[1]) .. "," .. tostring(coord[2])] = not blackTiles[tostring(coord[1]) .. "," .. tostring(coord[2])]
end

local count = 0
for _, black in pairs(blackTiles) do
    if black then
        count = count + 1
    end
end

print(count)
file:close()
