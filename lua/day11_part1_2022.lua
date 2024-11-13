
function parseInput(filename)
    local monkeys = {}
    local currentMonkey = nil

    for line in io.lines(filename) do
        line = line:match("^%s*(.-)%s*$")  -- Trim whitespace
        if line:match("^Monkey %d+:$") then
            currentMonkey = {
                items = {},
                operation = nil,
                test = nil,
                trueTarget = nil,
                falseTarget = nil,
                inspections = 0
            }
            table.insert(monkeys, currentMonkey)
        elseif line:match("Starting items:") then
            for num in line:gmatch("%d+") do
                table.insert(currentMonkey.items, tonumber(num))
            end
        elseif line:match("Operation:") then
            currentMonkey.operation = line:match("new = (.+)")
        elseif line:match("Test:") then
            currentMonkey.test = tonumber(line:match("divisible by (%d+)"))
        elseif line:match("If true:") then
            currentMonkey.trueTarget = tonumber(line:match("throw to monkey (%d+)"))
        elseif line:match("If false:") then
            currentMonkey.falseTarget = tonumber(line:match("throw to monkey (%d+)"))
        end
    end
    return monkeys
end

function evaluateOperation(operation, old)
    local op = operation:gsub("old", tostring(old))
    return load("return " .. op)()
end

function simulateRounds(monkeys, rounds)
    for _ = 1, rounds do
        for _, monkey in ipairs(monkeys) do
            while #monkey.items > 0 do
                local item = table.remove(monkey.items, 1)
                monkey.inspections = monkey.inspections + 1

                -- Perform operation and reduce worry
                item = math.floor(evaluateOperation(monkey.operation, item) / 3)

                -- Test and throw
                local targetMonkey = item % monkey.test == 0 
                    and monkey.trueTarget 
                    or monkey.falseTarget
                
                table.insert(monkeys[targetMonkey + 1].items, item)
            end
        end
    end
end

function calculateMonkeyBusiness(monkeys)
    table.sort(monkeys, function(a, b) 
        return a.inspections > b.inspections 
    end)
    return monkeys[1].inspections * monkeys[2].inspections
end

-- Main program
local monkeys = parseInput("input.txt")
simulateRounds(monkeys, 20)
print(calculateMonkeyBusiness(monkeys))
