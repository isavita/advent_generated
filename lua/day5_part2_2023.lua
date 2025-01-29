
local seedRanges = {}
local maps = {}

local function readInput()
    local file = io.open("input.txt", "r")
    local currentRanges = {}
    for line in file:lines() do
        if line:find("map:") then
            if #currentRanges > 0 then
                table.insert(maps, currentRanges)
                currentRanges = {}
            end
        elseif line:find("seeds:") then
            local seedStrs = {}
            local idx = line:find(" ")
            line = line:sub(idx+1)
            for num in line:gmatch("%d+") do
                table.insert(seedStrs, tonumber(num))
            end
            for i = 1, #seedStrs, 2 do
                table.insert(seedRanges, {seedStrs[i], seedStrs[i+1]})
            end
        else
            local nums = {}
            for num in line:gmatch("%d+") do
                table.insert(nums, tonumber(num))
            end
            if #nums == 3 then
                table.insert(currentRanges, {srcStart = nums[2], destStart = nums[1], length = nums[3]})
            end
        end
    end
    if #currentRanges > 0 then
        table.insert(maps, currentRanges)
    end
    file:close()
end

local function reverseConvert(number, mapSet)
    for i = #mapSet, 1, -1 do
        local r = mapSet[i]
        if number >= r.destStart and number < r.destStart + r.length then
            return r.srcStart + (number - r.destStart)
        end
    end
    return number
end

local function isInSeedRanges(number)
    for _, r in ipairs(seedRanges) do
        if number >= r[1] and number < r[1] + r[2] then
            return true
        end
    end
    return false
end

local function main()
    readInput()
    local location = 0
    while true do
        local seed = location
        for i = #maps, 1, -1 do
            seed = reverseConvert(seed, maps[i])
        end
        if isInSeedRanges(seed) then
            print(location)
            return
        end
        location = location + 1
    end
end

main()
