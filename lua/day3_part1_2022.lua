local function item_priority(item)
    if item >= 'a' and item <= 'z' then
        return string.byte(item) - string.byte('a') + 1
    else
        return string.byte(item) - string.byte('A') + 27
    end
end

local sum = 0
local file = io.open("input.txt", "r")
if file then
    for line in file:lines() do
        local half = #line // 2
        local first_compartment = line:sub(1, half)
        local second_compartment = line:sub(half + 1)

        local compartment_map = {}
        for i = 1, #first_compartment do
            local item = first_compartment:sub(i, i)
            compartment_map[item] = (compartment_map[item] or 0) + 1
        end
        for i = 1, #second_compartment do
            local item = second_compartment:sub(i, i)
            if compartment_map[item] then
                sum = sum + item_priority(item)
                break
            end
        end
    end
    file:close()
else
    print("Error opening file")
end

print(sum)