
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    os.exit()
end

local inputData = {}
for line in file:lines() do
    for val in line:gmatch("[^,]+") do
        table.insert(inputData, tonumber(val))
    end
end
file:close()

inputData[2] = 12
inputData[3] = 2

local function executeProgram(data)
    for i = 1, #data - 2, 4 do
        local opcode = data[i]
        local pos1 = data[i + 1]
        local pos2 = data[i + 2]
        local pos3 = data[i + 3]

        if opcode == 1 then
            data[pos3 + 1] = data[pos1 + 1] + data[pos2 + 1]
        elseif opcode == 2 then
            data[pos3 + 1] = data[pos1 + 1] * data[pos2 + 1]
        elseif opcode == 99 then
            return data[1]
        else
            error("Invalid opcode")
        end
    end

    return data[1]
end

local result = executeProgram(inputData)

print(result)
