
local total = 0

local file = io.open("input.txt", "r")
for line in file:lines() do
    local trimmed_line = line:gsub("%s+", " "):gsub("^ ", ""):gsub(" $", "")
    if trimmed_line == "" then
        goto continue_line
    end

    local target_str, nums_str = trimmed_line:match("^([^:]+):%s*(.+)$")
    if not target_str or nums_str == "" then
        goto continue_line
    end

    local target = tonumber(target_str)
    if not target then
        goto continue_line
    end

    local nums = {}
    for num_str in nums_str:gmatch("%S+") do
        local num = tonumber(num_str)
        if num then
            table.insert(nums, num)
        end
    end

    if #nums == 0 then
        goto continue_line
    end

    if #nums == 1 then
        if nums[1] == target then
            total = total + target
        end
        goto continue_line
    end

    local current = {}
    current[nums[1]] = true

    for i = 2, #nums do
        local num = nums[i]
        local next_current = {}

        for val in pairs(current) do
            local add = val + num
            next_current[add] = true

            local mul = val * num
            next_current[mul] = true

            local s_val = tostring(val)
            local s_num = tostring(num)
            local concat_str = s_val .. s_num
            local concat_num = tonumber(concat_str)
            next_current[concat_num] = true
        end

        current = next_current

        if not next(current) then
            break
        end
    end

    if current[target] then
        total = total + target
    end

    ::continue_line::
end
file:close()
print(total)
