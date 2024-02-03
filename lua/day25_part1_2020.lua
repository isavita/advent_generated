
function transform(subjectNumber, loopSize)
    local value = 1
    for i = 1, loopSize do
        value = (value * subjectNumber) % 20201227
    end
    return value
end

function findLoopSize(publicKey)
    local value = 1
    local loopSize = 0
    while value ~= publicKey do
        value = (value * 7) % 20201227
        loopSize = loopSize + 1
    end
    return loopSize
end

local file = io.open("input.txt", "r")
if not file then
    error("Error opening file")
end

local cardPublicKey = tonumber(file:read("*line"))
local doorPublicKey = tonumber(file:read("*line"))

local cardLoopSize = findLoopSize(cardPublicKey)
local encryptionKey = transform(doorPublicKey, cardLoopSize)

print(encryptionKey)
