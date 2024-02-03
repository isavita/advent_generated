
local file = io.open("input.txt", "r")
if not file then
    print("Error opening file")
    return
end

local totalCount = 0
local groupAnswers = {}
local groupSize = 0

for line in file:lines() do
    if line == "" then
        for _, count in pairs(groupAnswers) do
            if count == groupSize then
                totalCount = totalCount + 1
            end
        end
        groupAnswers = {}
        groupSize = 0
    else
        groupSize = groupSize + 1
        for i = 1, #line do
            local question = line:sub(i, i)
            if not groupAnswers[question] then
                groupAnswers[question] = 1
            else
                groupAnswers[question] = groupAnswers[question] + 1
            end
        end
    end
end

for _, count in pairs(groupAnswers) do
    if count == groupSize then
        totalCount = totalCount + 1
    end
end

print(totalCount)
file:close()
