
local file = io.open("input.txt", "r")
local input = file:read("*line")
file:close()

function getDecompressedLengthV2(input)
    return decompress(input, 1, #input)
end

function decompress(input, start, finish)
    local length = 0
    local i = start

    while i <= finish do
        local s, e, charCount, repeatCount = string.find(input:sub(i, finish), "%((%d+)x(%d+)%)")
        if s then
            charCount = tonumber(charCount)
            repeatCount = tonumber(repeatCount)
            local nextIndex = i + e
            length = length + repeatCount * decompress(input, nextIndex, nextIndex + charCount - 1)
            i = nextIndex + charCount
        else
            length = length + 1
            i = i + 1
        end
    end

    return length
end

local decompressedLength = getDecompressedLengthV2(input)
print(decompressedLength)
