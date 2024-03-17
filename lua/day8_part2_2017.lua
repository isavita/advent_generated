-- Read input from "input.txt" file
local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Initialize registers
local registers = {}
local highest_value = 0

-- Process instructions
for _, line in ipairs(lines) do
    local parts = {}
    for part in string.gmatch(line, "%S+") do
        table.insert(parts, part)
    end
    local reg = parts[1]
    local op = parts[2]
    local amount = tonumber(parts[3])
    local cond_reg = parts[5]
    local cond_op = parts[6]
    local cond_val = tonumber(parts[7])

    -- Check condition
    local cond = false
    if cond_op == ">" then
        cond = (registers[cond_reg] or 0) > cond_val
    elseif cond_op == ">=" then
        cond = (registers[cond_reg] or 0) >= cond_val
    elseif cond_op == "<" then
        cond = (registers[cond_reg] or 0) < cond_val
    elseif cond_op == "<=" then
        cond = (registers[cond_reg] or 0) <= cond_val
    elseif cond_op == "==" then
        cond = (registers[cond_reg] or 0) == cond_val
    elseif cond_op == "!=" then
        cond = (registers[cond_reg] or 0) ~= cond_val
    end

    if cond then
        if op == "inc" then
            registers[reg] = (registers[reg] or 0) + amount
        elseif op == "dec" then
            registers[reg] = (registers[reg] or 0) - amount
        end

        -- Update highest value
        if registers[reg] > highest_value then
            highest_value = registers[reg]
        end
    end
end

-- Print the highest value
print(highest_value)