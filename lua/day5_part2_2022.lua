-- Function to read the input from the file
function readInput(filename)
    local file = io.open(filename, "r")
    if not file then return nil end
    local content = file:read("*all")
    file:close()
    return content
end

-- Function to parse the input content into stacks and instructions
function parseInput(content)
    local stacks = {}
    local instructions = {}
    local parseStacks = true

    for line in content:gmatch("[^\r\n]+") do
        if line:match("^move") then
            parseStacks = false
        end

        if parseStacks then
            -- Parse stack lines
            for stackIndex, crate in line:gmatch("()%[([A-Z])%]") do
                local stackNum = stackIndex // 4 + 1
                stacks[stackNum] = stacks[stackNum] or {}
                table.insert(stacks[stackNum], 1, crate)
            end
        else
            -- Parse instructions
            local num, from, to = line:match("move (%d+) from (%d+) to (%d+)")
            if num and from and to then
                table.insert(instructions, {tonumber(num), tonumber(from), tonumber(to)})
            end
        end
    end

    return stacks, instructions
end

-- Function to execute the instructions with CrateMover 9001
function executeInstructions(stacks, instructions)
    for _, instruction in ipairs(instructions) do
        local num, from, to = table.unpack(instruction)
        local movingCrates = {}

        -- Pop crates from 'from' stack
        for _ = 1, num do
            table.insert(movingCrates, table.remove(stacks[from]))
        end

        -- Push crates to 'to' stack in the same order
        for i = #movingCrates, 1, -1 do
            table.insert(stacks[to], movingCrates[i])
        end
    end
end

-- Function to get the top crates from each stack and form the result string
function getTopCrates(stacks)
    local result = ""
    for i = 1, #stacks do
        if #stacks[i] > 0 then
            result = result .. stacks[i][#stacks[i]]
        end
    end
    return result
end

-- Main function
function main()
    local content = readInput("input.txt")
    if not content then
        print("Failed to read input file")
        return
    end

    local stacks, instructions = parseInput(content)
    executeInstructions(stacks, instructions)
    local result = getTopCrates(stacks)
    print(result)
end

main()
