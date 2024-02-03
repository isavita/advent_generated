
local file = io.open("input.txt", "r")
local input = file:read("*a")
file:close()

local targetSue = {
    children = 3,
    cats = 7,
    samoyeds = 2,
    pomeranians = 3,
    akitas = 0,
    vizslas = 0,
    goldfish = 5,
    trees = 3,
    cars = 2,
    perfumes = 1
}

function auntSue(input)
    for line in input:gmatch("[^\n]+") do
        local sueNum, thing1, amount1, thing2, amount2, thing3, amount3 = line:match("Sue (%d+): (%a+): (%d+), (%a+): (%d+), (%a+): (%d+)")
        thing1 = thing1:gsub(":", "")
        thing2 = thing2:gsub(":", "")
        thing3 = thing3:gsub(":", "")

        local readingsMap = {}
        readingsMap[thing1] = tonumber(amount1)
        readingsMap[thing2] = tonumber(amount2)
        readingsMap[thing3] = tonumber(amount3)

        local allRulesMatched = true

        for _, check in ipairs({"cats", "trees"}) do
            if readingsMap[check] and readingsMap[check] <= targetSue[check] then
                allRulesMatched = false
            end
            readingsMap[check] = nil
        end

        for _, check in ipairs({"pomeranians", "goldfish"}) do
            if readingsMap[check] and readingsMap[check] >= targetSue[check] then
                allRulesMatched = false
            end
            readingsMap[check] = nil
        end

        for thing, amount in pairs(readingsMap) do
            if targetSue[thing] ~= amount then
                allRulesMatched = false
            end
        end

        if allRulesMatched then
            return sueNum
        end
    end

    error("expect return from loop")
end

print(auntSue(input))
