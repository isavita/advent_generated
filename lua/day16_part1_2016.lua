
local file = io.open("input.txt", "r")
local initialState = file:read("*line")
file:close()

local diskLength = 272

function generateData(initialState, length)
    local data = initialState
    while #data < length do
        local b = ""
        for i = #data, 1, -1 do
            if string.sub(data, i, i) == "0" then
                b = b .. "1"
            else
                b = b .. "0"
            end
        end
        data = data .. "0" .. b
    end
    return string.sub(data, 1, length)
end

function calculateChecksum(data)
    while #data % 2 == 0 do
        local b = ""
        for i = 1, #data, 2 do
            if string.sub(data, i, i) == string.sub(data, i + 1, i + 1) then
                b = b .. "1"
            else
                b = b .. "0"
            end
        end
        data = b
    end
    return data
end

local data = generateData(initialState, diskLength)
local checksum = calculateChecksum(data)
print("Checksum:", checksum)
