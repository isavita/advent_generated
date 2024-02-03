
local file = io.open("input.txt", "r")
local seeds = {}
local currentRanges = {}
local maps = {}

for line in file:lines() do
    if string.find(line, "map:") then
        if #currentRanges > 0 then
            table.insert(maps, currentRanges)
            currentRanges = {}
        end
    elseif string.sub(line, 1, 6) == "seeds:" then
        local seedStrs = string.gmatch(line, "%S+")
        for s in seedStrs do
            table.insert(seeds, tonumber(s))
        end
    else
        local numbers = {}
        for num in string.gmatch(line, "%S+") do
            table.insert(numbers, tonumber(num))
        end
        if #numbers == 3 then
            table.insert(currentRanges, {srcStart = numbers[2], destStart = numbers[1], length = numbers[3]})
        end
    end
end
table.insert(maps, currentRanges)

local minLocation = -1
for _, seed in ipairs(seeds) do
    local location = seed
    for _, m in ipairs(maps) do
        for _, r in ipairs(m) do
            if location >= r.srcStart and location < r.srcStart + r.length then
                location = r.destStart + (location - r.srcStart)
                break
            end
        end
    end

    if minLocation == -1 or location < minLocation then
        minLocation = location
    end
end

print(minLocation)
