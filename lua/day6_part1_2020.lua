-- Function to read the input file and process the data
function processCustomsDeclarationForms(filename)
    local file = io.open(filename, "r")  -- Open the file for reading
    if not file then
        print("Failed to open file")
        return
    end

    local totalYesCount = 0
    local groupAnswers = {}
    
    for line in file:lines() do
        if line ~= "" then
            -- Collect answers from each person in the group
            for char in line:gmatch(".") do
                groupAnswers[char] = true
            end
        else
            -- End of a group, process the collected answers
            totalYesCount = totalYesCount + countYesAnswers(groupAnswers)
            groupAnswers = {}  -- Reset for next group
        end
    end

    -- Process the last group if the file doesn't end with a blank line
    if next(groupAnswers) ~= nil then
        totalYesCount = totalYesCount + countYesAnswers(groupAnswers)
    end

    file:close()
    return totalYesCount
end

-- Function to count the number of questions answered "yes" in a group
function countYesAnswers(groupAnswers)
    local count = 0
    for _ in pairs(groupAnswers) do
        count = count + 1
    end
    return count
end

-- Main execution
local filename = "input.txt"
local result = processCustomsDeclarationForms(filename)
print("Total 'yes' answers:", result)