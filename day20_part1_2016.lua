
function readIPRanges(filename)
    local file = io.open(filename, "r")
    local ranges = {}
    
    for line in file:lines() do
        local parts = {}
        for part in line:gmatch("%d+") do
            table.insert(parts, tonumber(part))
        end
        table.insert(ranges, {start = parts[1], finish = parts[2]})
    end
    
    file:close()
    return ranges
end

function findUnblockedIP(ranges)
    local currentIP = 0
    for i = 1, #ranges do
        local r = ranges[i]
        if r.start > currentIP then
            return currentIP
        end
        if r.finish >= currentIP then
            currentIP = r.finish + 1
        end
    end
    return currentIP
end

local ipRanges = readIPRanges("input.txt")
table.sort(ipRanges, function(a, b) return a.start < b.start end)

local unblockedIP = findUnblockedIP(ipRanges)
print(unblockedIP)
