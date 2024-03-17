local function filter_by_bit(values, bit_index, keep)
    local filtered = {}
    for _, val in ipairs(values) do
        if val:sub(bit_index, bit_index) == keep then
            table.insert(filtered, val)
        end
    end
    return filtered
end

local function filter_values(values, criteria)
    for i = 1, #values[1] do
        local zeros, ones = 0, 0
        for _, val in ipairs(values) do
            if val:sub(i, i) == '0' then
                zeros = zeros + 1
            else
                ones = ones + 1
            end
        end
        local keep = criteria(zeros, ones)
        values = filter_by_bit(values, i, keep)
        if #values == 1 then
            break
        end
    end
    return values[1]
end

local file = io.open("input.txt", "r")
local values = {}
for line in file:lines() do
    table.insert(values, line)
end
file:close()

local oxygen_generator_rating = filter_values(values, function(zeros, ones)
    return zeros > ones and '0' or '1'
end)
local oxygen_generator_rating_int = tonumber(oxygen_generator_rating, 2)

local co2_scrubber_rating = filter_values(values, function(zeros, ones)
    return zeros <= ones and '0' or '1'
end)
local co2_scrubber_rating_int = tonumber(co2_scrubber_rating, 2)

print(oxygen_generator_rating_int * co2_scrubber_rating_int)