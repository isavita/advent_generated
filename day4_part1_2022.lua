
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local count = 0
for line in file:lines() do
    local ranges = {}
    for range in string.gmatch(line, "%d+-%d+") do
        table.insert(ranges, range)
    end

    if #ranges ~= 2 then
        goto continue
    end

    local start1, end1 = string.match(ranges[1], "(%d+)-(%d+)")
    local start2, end2 = string.match(ranges[2], "(%d+)-(%d+)")

    if (tonumber(start1) <= tonumber(start2) and tonumber(end1) >= tonumber(end2)) or (tonumber(start2) <= tonumber(start1) and tonumber(end2) >= tonumber(end1)) then
        count = count + 1
    end

    ::continue::
end

file:close()
print(count)
