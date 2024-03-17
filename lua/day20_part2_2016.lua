local function parse_input()
    local ranges = {}
    for line in io.lines("input.txt") do
        local start, stop = line:match("(%d+)-(%d+)")
        start, stop = tonumber(start), tonumber(stop)
        table.insert(ranges, {start, stop})
    end
    table.sort(ranges, function(a, b) return a[1] < b[1] end)
    return ranges
end

local function find_lowest_allowed_ip(ranges)
    local lowest_allowed = 0
    for _, range in ipairs(ranges) do
        if range[1] > lowest_allowed then
            return lowest_allowed
        end
        lowest_allowed = math.max(lowest_allowed, range[2] + 1)
    end
    return lowest_allowed
end

local function count_allowed_ips(ranges)
    local allowed = 0
    local prev_end = -1
    for _, range in ipairs(ranges) do
        if range[1] > prev_end + 1 then
            allowed = allowed + range[1] - prev_end - 1
        end
        prev_end = math.max(prev_end, range[2])
    end
    return allowed + 4294967296 - prev_end - 1
end

local ranges = parse_input()
print(find_lowest_allowed_ip(ranges))
print(count_allowed_ips(ranges))