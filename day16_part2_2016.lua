
local function readInitialState(filename)
    local file = io.open(filename, "r")
    local initialState = file:read("*line")
    file:close()
    return initialState
end

local function generateData(initialState, length)
    local data = initialState
    while #data < length do
        local b = {}
        for i = #data, 1, -1 do
            if data:sub(i, i) == "0" then
                table.insert(b, "1")
            else
                table.insert(b, "0")
            end
        end
        data = data .. "0" .. table.concat(b)
    end
    return data:sub(1, length)
end

local function calculateChecksum(data)
    while #data % 2 == 0 do
        local b = {}
        for i = 1, #data, 2 do
            if data:sub(i, i) == data:sub(i + 1, i + 1) then
                table.insert(b, "1")
            else
                table.insert(b, "0")
            end
        end
        data = table.concat(b)
    end
    return data
end

local initialState = readInitialState("input.txt")
local diskLength = 35651584
local data = generateData(initialState, diskLength)
local checksum = calculateChecksum(data)
print("Checksum:", checksum)
