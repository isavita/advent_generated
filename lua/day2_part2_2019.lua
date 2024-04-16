local function execute(memory)
    local i = 1
    while true do
        local opcode = memory[i]
        if opcode == 99 then
            return memory[1]
        elseif opcode == 1 then
            memory[memory[i + 3] + 1] = memory[memory[i + 1] + 1] + memory[memory[i + 2] + 1]
        elseif opcode == 2 then
            memory[memory[i + 3] + 1] = memory[memory[i + 1] + 1] * memory[memory[i + 2] + 1]
        end
        i = i + 4
    end
end

local file = io.open("input.txt", "r")
local data = file:read("*all")
file:close()

local strs = {}
for str in string.gmatch(data, "%d+") do
    table.insert(strs, tonumber(str))
end
local original = strs

for noun = 0, 99 do
    for verb = 0, 99 do
        local memory = {}
        for i, v in ipairs(original) do
            memory[i] = v
        end
        memory[2] = noun
        memory[3] = verb
        if execute(memory) == 19690720 then
            print(100 * noun + verb)
            os.exit()
        end
    end
end