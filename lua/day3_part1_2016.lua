
local file = io.open("input.txt", "r")
local validTriangles = 0

for line in file:lines() do
    local sides = {}
    for side in line:gmatch("%S+") do
        table.insert(sides, tonumber(side))
    end

    if #sides ~= 3 then
        print("Invalid input format")
    else
        local a, b, c = sides[1], sides[2], sides[3]
        if a + b > c and a + c > b and b + c > a then
            validTriangles = validTriangles + 1
        end
    end
end

print(validTriangles)
