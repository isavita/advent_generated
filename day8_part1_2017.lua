
-- Step 1: Read Input
file = io.open("input.txt", "r")
if not file then
    print("File reading error")
    return
end

-- Step 2: Initialize Registers
registers = {}

-- Step 3: Process Instructions
for line in file:lines() do
    parts = {}
    for part in line:gmatch("%S+") do
        table.insert(parts, part)
    end
    
    reg = parts[1]
    op = parts[2]
    amount = tonumber(parts[3])
    condReg = parts[5]
    condOp = parts[6]
    condVal = tonumber(parts[7])

    cond = false
    if condOp == ">" then
        cond = (registers[condReg] or 0) > condVal
    elseif condOp == ">=" then
        cond = (registers[condReg] or 0) >= condVal
    elseif condOp == "<" then
        cond = (registers[condReg] or 0) < condVal
    elseif condOp == "<=" then
        cond = (registers[condReg] or 0) <= condVal
    elseif condOp == "==" then
        cond = (registers[condReg] or 0) == condVal
    elseif condOp == "!=" then
        cond = (registers[condReg] or 0) ~= condVal
    end

    if cond then
        if op == "inc" then
            registers[reg] = (registers[reg] or 0) + amount
        elseif op == "dec" then
            registers[reg] = (registers[reg] or 0) - amount
        end
    end
end

-- Step 4: Find Max Value
maxValue = 0
for key, value in pairs(registers) do
    if value > maxValue then
        maxValue = value
    end
end

print(maxValue)
