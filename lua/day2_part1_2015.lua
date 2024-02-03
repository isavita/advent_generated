
local file = io.open("input.txt", "r")
local total = 0

for line in file:lines() do
    local dimensions = {}
    for dimension in string.gmatch(line, "%d+") do
        table.insert(dimensions, tonumber(dimension))
    end

    local l, w, h = dimensions[1], dimensions[2], dimensions[3]

    local side1 = l * w
    local side2 = w * h
    local side3 = h * l

    local smallest = math.min(side1, side2, side3)
    total = total + 2*side1 + 2*side2 + 2*side3 + smallest
end

file:close()
print(total)
