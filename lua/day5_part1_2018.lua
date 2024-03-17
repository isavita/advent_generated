function react(polymer)
    repeat
        local reacted = false
        for i = 1, #polymer - 1 do
            if polymer:sub(i, i) ~= polymer:sub(i + 1, i + 1) and
                (polymer:byte(i) + 32 == polymer:byte(i + 1) or
                    polymer:byte(i) - 32 == polymer:byte(i + 1)) then
                polymer = polymer:sub(1, i - 1) .. polymer:sub(i + 2)
                reacted = true
                break
            end
        end
    until not reacted
    return polymer
end

local file = io.open("input.txt", "r")
local polymer = file:read("*line")
file:close()

local result = react(polymer)
print(#result)