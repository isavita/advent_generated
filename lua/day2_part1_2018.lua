-- Helper function to count twos and threes
function count_twos_and_threes(id)
    local char_count = {}
    for char in id:gmatch(".") do
        char_count[char] = (char_count[char] or 0) + 1
    end

    local has_twos, has_threes = false, false
    for _, count in pairs(char_count) do
        if count == 2 then
            has_twos = true
        elseif count == 3 then
            has_threes = true
        end
    end
    return has_twos, has_threes
end

-- Read input from file
local file = io.open("input.txt", "r")
if not file then
    error("Failed to open input.txt")
end

local two_count, three_count = 0, 0

for line in file:lines() do
    local has_twos, has_threes = count_twos_and_threes(line)
    if has_twos then
        two_count = two_count + 1
    end
    if has_threes then
        three_count = three_count + 1
    end
end

file:close()

local checksum = two_count * three_count
print(checksum)