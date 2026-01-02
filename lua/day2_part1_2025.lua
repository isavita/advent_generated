
local f = assert(io.open("input.txt"))
local raw = f:read("*a"):gsub("[\r\n]", ""):match("^%s*(.-)%s*$")
f:close()

local ranges = {}
for part in raw:gmatch("[^,]+") do
    local s, e = part:match("([^%-]+)%-(.+)")
    ranges[#ranges + 1] = {tonumber(s), tonumber(e)}
end

local maxEnd = 0
for _, r in ipairs(ranges) do
    if r[2] > maxEnd then maxEnd = r[2] end
end
local maxK = math.ceil(#tostring(maxEnd) / 2)

local ids = {}
for k = 1, maxK do
    local pow10k = 10 ^ k
    local mult = pow10k + 1
    local minSeed = (k == 1) and 1 or 10 ^ (k - 1)
    local maxSeed = pow10k - 1
    for _, r in ipairs(ranges) do
        local sMin = (r[1] + mult - 1) // mult
        local sMax = r[2] // mult
        local low = sMin > minSeed and sMin or minSeed
        local high = sMax < maxSeed and sMax or maxSeed
        if low <= high then
            for cur = low, high do
                ids[cur * mult] = true
            end
        end
    end
end

local total = 0
for id in pairs(ids) do total = total + id end
print(total)
