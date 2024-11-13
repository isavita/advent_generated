
local function parse_input(filename)
    local monkeys = {}
    local current_monkey = nil

    for line in io.lines(filename) do
        line = line:match("^%s*(.-)%s*$")  -- Trim whitespace
        if line:match("^Monkey %d+:$") then
            current_monkey = {
                items = {},
                operation = nil,
                test_divisible = nil,
                true_target = nil,
                false_target = nil,
                inspections = 0
            }
            table.insert(monkeys, current_monkey)
        elseif line:match("Starting items:") then
            for num in line:match(": (.+)"):gmatch("%d+") do
                table.insert(current_monkey.items, tonumber(num))
            end
        elseif line:match("Operation:") then
            current_monkey.operation = line:match(": new = (.+)")
        elseif line:match("Test:") then
            current_monkey.test_divisible = tonumber(line:match("divisible by (%d+)"))
        elseif line:match("If true:") then
            current_monkey.true_target = tonumber(line:match("monkey (%d+)"))
        elseif line:match("If false:") then
            current_monkey.false_target = tonumber(line:match("monkey (%d+)"))
        end
    end
    return monkeys
end

local function solve_monkey_business(monkeys, rounds, worry_management)
    local lcm = 1
    for _, monkey in ipairs(monkeys) do
        lcm = lcm * monkey.test_divisible
    end

    for _ = 1, rounds do
        for _, monkey in ipairs(monkeys) do
            while #monkey.items > 0 do
                monkey.inspections = monkey.inspections + 1
                local item = table.remove(monkey.items, 1)
                
                -- Evaluate operation
                local old = item
                local new = load("return " .. monkey.operation:gsub("old", tostring(old)))()
                
                -- Manage worry levels
                new = worry_management(new, lcm)
                
                -- Decide target monkey
                local target = (new % monkey.test_divisible == 0) 
                    and monkey.true_target 
                    or monkey.false_target
                
                table.insert(monkeys[target + 1].items, new)
            end
        end
    end

    -- Sort monkeys by inspections
    table.sort(monkeys, function(a, b) return a.inspections > b.inspections end)
    
    return monkeys[1].inspections * monkeys[2].inspections
end

local function part1(monkeys)
    return solve_monkey_business(monkeys, 20, function(worry, _) 
        return math.floor(worry / 3) 
    end)
end

local function part2(monkeys)
    return solve_monkey_business(monkeys, 10000, function(worry, lcm) 
        return worry % lcm 
    end)
end

local function main()
    local monkeys = parse_input("input.txt")
    
    -- Deep copy for part 2
    local monkeys_copy = {}
    for _, monkey in ipairs(monkeys) do
        local new_monkey = {
            items = {},
            operation = monkey.operation,
            test_divisible = monkey.test_divisible,
            true_target = monkey.true_target,
            false_target = monkey.false_target,
            inspections = 0
        }
        for _, item in ipairs(monkey.items) do
            table.insert(new_monkey.items, item)
        end
        table.insert(monkeys_copy, new_monkey)
    end

    print("Part 1:", part1(monkeys))
    print("Part 2:", part2(monkeys_copy))
end

main()
