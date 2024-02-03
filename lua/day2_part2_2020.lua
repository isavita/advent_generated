
validCount = 0

for line in io.lines("input.txt") do
    i = string.find(line, ":")
    if i then
        policy = string.sub(line, 1, i-1)
        password = string.sub(line, i+2)
        min, max, char = string.match(policy, "(%d+)-(%d+) (%a)")
        min = tonumber(min)
        max = tonumber(max)
        char = string.byte(char)
        if (string.byte(password, min) == char) ~= (string.byte(password, max) == char) then
            validCount = validCount + 1
        end
    end
end

print(validCount)
