local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local function count_chars(str)
    local counts = {}
    for c in str:gmatch("%a") do
        counts[c] = (counts[c] or 0) + 1
    end
    return counts
end

local function is_real_room(name, checksum)
    local counts = count_chars(name:gsub("-", ""))
    local sorted_counts = {}
    for c, count in pairs(counts) do
        table.insert(sorted_counts, {c, count})
    end
    table.sort(sorted_counts, function(a, b) return a[2] > b[2] or (a[2] == b[2] and a[1] < b[1]) end)
    local real_checksum = ""
    for i = 1, 5 do
        real_checksum = real_checksum .. sorted_counts[i][1]
    end
    return real_checksum == checksum
end

local sum = 0
for _, line in ipairs(lines) do
    local name, sector_id, checksum = line:match("(%a+%-[%a-]+)%-(%d+)%[(%a+)%]")
    if is_real_room(name, checksum) then
        sum = sum + tonumber(sector_id)
    end
end

print(sum)