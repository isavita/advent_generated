
-- Step 1: Read Input
file = io.open("input.txt", "r")
if not file then
    print("File reading error")
    return
end

-- Step 2: Initialize Variables
score = 0
depth = 0
inGarbage = false
cancelNext = false

-- Step 3: Process Stream
for line in file:lines() do
    for i = 1, #line do
        local ch = line:sub(i, i)
        if cancelNext then
            cancelNext = false
        elseif inGarbage then
            if ch == '!' then
                cancelNext = true
            elseif ch == '>' then
                inGarbage = false
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
    end
end

-- Step 4: Print Score
print(score)
