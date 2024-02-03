
local file = io.open("input.txt", "r")
local messages = {}
for line in file:lines() do
    table.insert(messages, line)
end
file:close()

function getOriginalMessage(messages)
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

    local originalMessage = {}
    for _, charCount in ipairs(count) do
        table.insert(originalMessage, getLeastCommonChar(charCount))
    end

    return table.concat(originalMessage)
end

function getLeastCommonChar(count)
    local minChar
    local minCount = math.huge
    for char, cnt in pairs(count) do
        if cnt < minCount then
            minCount = cnt
            minChar = char
        end
    end
    return minChar
end

local originalMessage = getOriginalMessage(messages)
print(originalMessage)
