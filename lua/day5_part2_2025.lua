
local ranges = {}
for line in io.lines("input.txt") do
    line = line:match("^%s*(.-)%s*$")
    if line == "" then break end
    local a, b = line:match("^(%-?%d+)%s*%-%s*(%-?%d+)$")
    if not a then os.exit(1) end
    a = tonumber(a)
    b = tonumber(b)
    if a > b then a, b = b, a end
    ranges[#ranges + 1] = {a, b}
end

if #ranges == 0 then
    print("Total fresh IDs: 0")
    os.exit()
end

table.sort(ranges, function(x, y) return x[1] < y[1] or (x[1] == y[1] and x[2] < y[2]) end)

local total, curMin, curMax = 0, ranges[1][1], ranges[1][2]
for i = 2, #ranges do
    local mn, mx = ranges[i][1], ranges[i][2]
    if mn <= curMax then
        if mx > curMax then curMax = mx end
    else
        total = total + (curMax - curMin + 1)
        curMin, curMax = mn, mx
    end
end
total = total + (curMax - curMin + 1)
print("Total fresh IDs: " .. total)
