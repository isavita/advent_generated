
local file = io.open("input.txt", "r")
local totalRibbon = 0

for line in file:lines() do
    local dimensions = {}
    for dimension in string.gmatch(line, "%d+") do
        table.insert(dimensions, tonumber(dimension))
    end

    local l, w, h = dimensions[1], dimensions[2], dimensions[3]

    local bow = l * w * h

    local sides = {l, w, h}
    table.sort(sides)
    local wrap = 2 * sides[1] + 2 * sides[2]

    totalRibbon = totalRibbon + bow + wrap
end

file:close()

print(totalRibbon)
