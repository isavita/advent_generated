
local mask = 0xFFFFFF
local total = 0
local file = io.open("input.txt", "r")
for line in file:lines() do
    if line == "" then
        goto continue
    end
    local num = tonumber(line)
    if num == nil then
        goto continue
    end
    local s = num & mask  -- Ensure initial s is within 24 bits
    for _ = 1, 2000 do
        -- Step 1: multiply by 64 (shift left by 6)
        local x = s << 6
        s = ((s | x) - (s & x)) & mask
        -- Step 2: divide by 32 (right shift by 5)
        x = s >> 5
        s = ((s | x) - (s & x)) & mask
        -- Step 3: multiply by 2048 (shift left by 11)
        x = s << 11
        s = ((s | x) - (s & x)) & mask
    end
    total = total + s
    ::continue::
end
file:close()
print(total)
