
local file = io.open("input.txt", "r")
local imageData = file:read("*all")
file:close()

local width, height = 25, 6
local layerSize = width * height

local minZeros = layerSize + 1
local result = 0

for i = 1, #imageData, layerSize do
    local layer = string.sub(imageData, i, i + layerSize - 1)
    local zeroCount, oneCount, twoCount = 0, 0, 0

    for j = 1, #layer do
        local pixel = string.sub(layer, j, j)
        if pixel == "0" then
            zeroCount = zeroCount + 1
        elseif pixel == "1" then
            oneCount = oneCount + 1
        elseif pixel == "2" then
            twoCount = twoCount + 1
        end
    end

    if zeroCount < minZeros then
        minZeros = zeroCount
        result = oneCount * twoCount
    end
end

print(result)
