
function parseInput(filePath)
    local file = io.open(filePath, "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    local initialState = string.sub(lines[1], #lines[1]-1, #lines[1]-1)
    local steps = tonumber(string.match(lines[2], "%d+"))

    local states = {}
    for i = 4, #lines, 10 do
        local state = string.sub(lines[i], #lines[i]-1, #lines[i]-1)
        local value0 = tonumber(string.sub(lines[i+2], #lines[i+2]-1, #lines[i+2]-1))
        local move0 = 1
        if string.find(lines[i+3], "left.") then
            move0 = -1
        end
        local nextState0 = string.sub(lines[i+4], #lines[i+4]-1, #lines[i+4]-1)
        local value1 = tonumber(string.sub(lines[i+6], #lines[i+6]-1, #lines[i+6]-1))
        local move1 = 1
        if string.find(lines[i+7], "left.") then
            move1 = -1
        end
        local nextState1 = string.sub(lines[i+8], #lines[i+8]-1, #lines[i+8]-1)
        states[state] = {[0] = {value0, move0, nextState0}, [1] = {value1, move1, nextState1}}
    end
    return initialState, steps, states
end

function runTuringMachine(filePath)
    local state, steps, states = parseInput(filePath)
    local tape = {}
    local cursor = 0
    local checksum = 0

    for i = 1, steps do
        local value = tape[cursor] or 0
        local newValue = states[state][value][1]
        local move = states[state][value][2]
        local nextState = states[state][value][3]

        tape[cursor] = newValue
        cursor = cursor + move
        state = nextState
    end

    for _, v in pairs(tape) do
        checksum = checksum + v
    end
    return checksum
end

local result = runTuringMachine("input.txt")
print(result)
