
local file = io.open("input.txt", "r")
local instructions = {}
for line in file:lines() do
    table.insert(instructions, line)
end
file:close()

local keypad = {
    {1, 2, 3},
    {4, 5, 6},
    {7, 8, 9}
}
local x, y = 2, 2
local code = ""

for _, instruction in ipairs(instructions) do
    for i = 1, #instruction do
        local move = instruction:sub(i, i)
        if move == "U" and x > 1 then
            x = x - 1
        elseif move == "D" and x < 3 then
            x = x + 1
        elseif move == "L" and y > 1 then
            y = y - 1
        elseif move == "R" and y < 3 then
            y = y + 1
        end
    end
    code = code .. keypad[x][y]
end

print(code)
