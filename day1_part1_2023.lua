
local sum = 0

for line in io.lines("input.txt") do
    if line ~= "" then
        local firstDigit, lastDigit = -1, -1

        for i = 1, #line do
            local char = string.sub(line, i, i)
            if tonumber(char) then
                if firstDigit == -1 then
                    firstDigit = tonumber(char)
                end
                lastDigit = tonumber(char)
            end
        end

        if firstDigit ~= -1 and lastDigit ~= -1 then
            local value = tonumber(tostring(firstDigit) .. tostring(lastDigit))
            sum = sum + value
        end
    end
end

print(sum)
