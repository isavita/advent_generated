
validCount = 0

file = io.open("input.txt", "r")
for line in file:lines() do
    i = string.find(line, ":")
    if i then
        policy = string.sub(line, 1, i-1)
        password = string.sub(line, i+2)
        min, max, char = string.match(policy, "(%d+)-(%d+) (%a)")
        count = 0
        for i=1, #password do
            if string.sub(password, i, i) == char then
                count = count + 1
            end
        end
        if count >= tonumber(min) and count <= tonumber(max) then
            validCount = validCount + 1
        end
    end
end

print(validCount)
