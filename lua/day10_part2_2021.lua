local file = io.open("input.txt", "r")
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

local points = {
    [")"] = 3,
    ["]"] = 57,
    ["}"] = 1197,
    [">"] = 25137
}

local score = 0
local incomplete_lines = {}
for _, line in ipairs(lines) do
    local stack = {}
    local is_corrupted = false
    for i = 1, #line do
        local char = line:sub(i, i)
        if char == "(" or char == "[" or char == "{" or char == "<" then
            table.insert(stack, char)
        else
            local top = table.remove(stack)
            if (top == "(" and char ~= ")") or
               (top == "[" and char ~= "]") or
               (top == "{" and char ~= "}") or
               (top == "<" and char ~= ">") then
                score = score + points[char]
                is_corrupted = true
                break
            end
        end
    end
    if not is_corrupted then
        table.insert(incomplete_lines, line)
    end
end

print("Part 1 answer:", score)

local completion_scores = {}
for _, line in ipairs(incomplete_lines) do
    local stack = {}
    local completion = ""
    for i = 1, #line do
        local char = line:sub(i, i)
        if char == "(" or char == "[" or char == "{" or char == "<" then
            table.insert(stack, char)
        else
            table.remove(stack)
        end
    end
    while #stack > 0 do
        local top = table.remove(stack)
        if top == "(" then
            completion = completion .. ")"
        elseif top == "[" then
            completion = completion .. "]"
        elseif top == "{" then
            completion = completion .. "}"
        elseif top == "<" then
            completion = completion .. ">"
        end
    end
    local score = 0
    for i = 1, #completion do
        local char = completion:sub(i, i)
        score = score * 5 + ({[")"] = 1, ["]"] = 2, ["}"] = 3, [">"] = 4})[char]
    end
    table.insert(completion_scores, score)
end

table.sort(completion_scores)
local middle_score = completion_scores[math.floor(#completion_scores / 2) + 1]
print("Part 2 answer:", middle_score)