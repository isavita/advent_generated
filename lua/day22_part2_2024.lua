
local mod = 1 << 24
local numSteps = 2000

local function nextSecret(s)
    local x = s * 64
    s = s ~ x
    s = s & (mod - 1)
    x = math.floor(s / 32)
    s = s ~ x
    s = s & (mod - 1)
    x = s * 2048
    s = s ~ x
    s = s & (mod - 1)
    return s
end

local function encodeChange4(c1, c2, c3, c4)
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19
end

local f = io.open("input.txt", "r")
if not f then
    error("Could not open input.txt")
end

local initials = {}
for line in f:lines() do
    if line ~= "" then
        table.insert(initials, tonumber(line))
    end
end
f:close()

local buyers = {}
for i, initVal in ipairs(initials) do
    local prices = {}
    local s = initVal
    for j = 0, numSteps do
        table.insert(prices, s % 10)
        if j < numSteps then
            s = nextSecret(s)
        end
    end
    local changes = {}
    for j = 1, numSteps do
        table.insert(changes, prices[j + 1] - prices[j])
    end
    buyers[i] = { prices = prices, changes = changes }
end

local patternCount = 19 * 19 * 19 * 19
local globalSum = {}

for i = 1, patternCount do
    globalSum[i] = 0
end

for _, b in ipairs(buyers) do
    local localPrice = {}
    for i = 1, patternCount do
        localPrice[i] = -1
    end
    for i = 1, numSteps - 3 do
        local c1, c2, c3, c4 = b.changes[i], b.changes[i + 1], b.changes[i + 2], b.changes[i + 3]
        if c1 >= -9 and c1 <= 9 and c2 >= -9 and c2 <= 9 and
           c3 >= -9 and c3 <= 9 and c4 >= -9 and c4 <= 9 then
            local idx = encodeChange4(c1, c2, c3, c4)
            if localPrice[idx] < 0 then
                localPrice[idx] = b.prices[i + 4]
            end
        end
    end
    for idx, p in ipairs(localPrice) do
        if p >= 0 then
            globalSum[idx] = globalSum[idx] + p
        end
    end
end

local ans = 0
for _, s in ipairs(globalSum) do
    if s > ans then
        ans = s
    end
end

print(ans)
