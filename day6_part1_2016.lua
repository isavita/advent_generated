
local file = io.open("input.txt", "r")
local messages = {}
for line in file:lines() do
    table.insert(messages, line)
end
file:close()

function getCorrectedMessage(messages)
    if #messages == 0 then
        return ""
    end
    local messageLength = #messages[1]
    local count = {}
    for i = 1, messageLength do
        count[i] = {}
    end

    for _, message in ipairs(messages) do
        for j = 1, messageLength do
            local char = message:sub(j, j)
            if count[j][char] == nil then
                count[j][char] = 1
            else
                count[j][char] = count[j][char] + 1
            end
        end
    end

    local correctedMessage = {}
    for _, charCount in ipairs(count) do
        table.insert(correctedMessage, getMostCommonChar(charCount))
    end

    return table.concat(correctedMessage)
end

function getMostCommonChar(count)
    local maxChar
    local maxCount = 0
    for char, cnt in pairs(count) do
        if cnt > maxCount then
            maxCount = cnt
            maxChar = char
        end
    end
    return maxChar
end

local correctedMessage = getCorrectedMessage(messages)
print(correctedMessage)
