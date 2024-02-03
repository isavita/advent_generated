
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

stars = {}
for i = 1, #lines do
    local x, y, vX, vY = lines[i]:match("position=<%s*(-?%d+),%s*(-?%d+)> velocity=<%s*(-?%d+),%s*(-?%d+)>")
    stars[i] = {x = tonumber(x), y = tonumber(y), vX = tonumber(vX), vY = tonumber(vY)}
end

local smallestT = 0
local smallestArea = 9223372036854775807
for t = 1, 100000 do
    local maxX = 0
    local maxY = 0
    local minX = 0
    local minY = 0
    
    for i = 1, #stars do
        local temp = stars[i]
        local x = temp.x + temp.vX*t
        if maxX < x then
            maxX = x
        elseif minX > x then
            minX = x
        end
        local y = temp.y + temp.vY*t
        if maxY < y then
            maxY = y
        elseif minY > y then
            minY = y
        end
    end
    
    local lenX = maxX - minX + 1
    local lenY = maxY - minY + 1
    local area = lenX + lenY
    
    if smallestArea > area then
        smallestArea = area
        smallestT = t
    end
end

print(smallestT)
