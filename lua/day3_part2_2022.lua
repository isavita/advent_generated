-- Function to get the priority of an item
local function item_priority(item)
    if item >= 'a' and item <= 'z' then
        return string.byte(item) - string.byte('a') + 1
    else
        return string.byte(item) - string.byte('A') + 27
    end
end

-- Main function
local function main()
    local file = io.open("input.txt", "r")
    if not file then
        print("Error opening file")
        return
    end

    local sum, group_line_counter = 0, 0
    local group_items = {{}, {}, {}}
    for line in file:lines() do
        local items_map = {}
        for i = 1, #line do
            local item = line:sub(i, i)
            items_map[item] = (items_map[item] or 0) + 1
        end
        group_items[group_line_counter + 1] = items_map
        group_line_counter = group_line_counter + 1

        if group_line_counter == 3 then
            local common_items = {}
            for item, _ in pairs(group_items[1]) do
                if group_items[2][item] and group_items[3][item] then
                    common_items[item] = true
                end
            end
            for item, _ in pairs(common_items) do
                sum = sum + item_priority(item)
                break -- Since we need only one common item per group
            end
            group_line_counter = 0
        end
    end

    file:close()
    print(sum)
end

main()