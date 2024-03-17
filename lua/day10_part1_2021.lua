-- Read the input from the file
local file = io.open("input.txt", "r")
if not file then
    error("Failed to open the input file")
end
local lines = {}
for line in file:lines() do
    table.insert(lines, line)
end
file:close()

-- Function to check a line
local function check_line(line)
    local pairings = {[')']=('('), [']']=('['), ['}']='{', ['>']=('<')}
    local scores = {[')']=3, [']']=57, ['}']=1197, ['>']=25137}
    local stack = {}

    for i=1, #line do
        local char = line:sub(i,i)
        if char == '(' or char == '[' or char == '{' or char == '<' then
            table.insert(stack, char)
        elseif char == ')' or char == ']' or char == '}' or char == '>' then
            if #stack == 0 or stack[#stack] ~= pairings[char] then
                return scores[char], true -- corrupted line
            end
            table.remove(stack)
        end
    end
    return 0, false -- line is not corrupted
end

-- Calculate the total score
local total_score = 0
for _, line in ipairs(lines) do
    local score, corrupted = check_line(line)
    if corrupted then
        total_score = total_score + score
    end
end

-- Print the answer
print(total_score)