local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local mask = ""
local mem = {}

for line in file:lines() do
    if string.match(line, "mask = (.*)") then
        mask = string.match(line, "mask = (.*)")
    else
        local address, value = string.match(line, "mem%[(%d+)%] = (%d+)")
        address = tonumber(address)
        value = tonumber(value)
        
        local result = 0
        for i = 1, 36 do
            local bitValue = 2^(36 - i)
            if string.sub(mask, i, i) == '1' then
                result = result | bitValue
            elseif string.sub(mask, i, i) == 'X' then
                result = result | (value & bitValue)
            end
        end
        
        mem[address] = result
    end
end

local sum = 0
for _, value in pairs(mem) do
    sum = sum + value
end

print(sum)

file:close()