
local function trim(s)
    return s:match("^%s*(.-)%s*$")
end

local function contains(r, x)
    local l, h = 1, #r
    while l <= h do
        local m = (l + h) // 2
        if x < r[m].min then
            h = m - 1
        elseif x > r[m].max then
            l = m + 1
        else
            return true
        end
    end
    return false
end

local f = io.open("input.txt", "r")
if not f then os.exit(1) end

local ranges = {}
local parsing = true
local fresh = 0

for line in f:lines() do
    line = trim(line)
    if line == "" then
        if parsing then
            parsing = false
            table.sort(ranges, function(a, b)
                if a.min ~= b.min then return a.min < b.min end
                return a.max < b.max
            end)
            local merged = {}
            for _, v in ipairs(ranges) do
                if #merged == 0 or v.min > merged[#merged].max then
                    merged[#merged + 1] = {min = v.min, max = v.max}
                elseif v.max > merged[#merged].max then
                    merged[#merged].max = v.max
                end
            end
            ranges = merged
        end
    elseif parsing then
        local a, b = line:match("^(%-?%d+)%s*%-%s*(%-?%d+)$")
        if not a then f:close(); os.exit(1) end
        ranges[#ranges + 1] = {min = tonumber(a), max = tonumber(b)}
    else
        local id = tonumber(line)
        if id and #ranges > 0 and contains(ranges, id) then fresh = fresh + 1 end
    end
end

f:close()
print("Number of fresh ingredients: " .. fresh)
