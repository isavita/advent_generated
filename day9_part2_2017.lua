
local file = io.open("input.txt", "r")
if not file then
    print("File reading error")
    return
end

local score = 0
local depth = 0
local inGarbage = false
local cancelNext = false
local garbageCount = 0

for line in file:lines() do
    for i = 1, #line do
        local ch = line:sub(i, i)
        if cancelNext then
            cancelNext = false
            goto continue
        end

        if inGarbage then
            if ch == '!' then
                cancelNext = true
            elseif ch == '>' then
                inGarbage = false
            else
                garbageCount = garbageCount + 1
            end
        else
            if ch == '{' then
                depth = depth + 1
            elseif ch == '}' then
                score = score + depth
                depth = depth - 1
            elseif ch == '<' then
                inGarbage = true
            end
        end

        ::continue::
    end
end

print(garbageCount)
