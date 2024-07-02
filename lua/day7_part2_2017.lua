-- Function to read and parse input
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    local programs = {}
    for line in file:lines() do
        local name, weight, children = line:match("(%w+) %((%d+)%)(.*)")
        weight = tonumber(weight)
        children = children and children:gsub(" %-> ", ""):gsub("%s", ""):split(",") or {}
        programs[name] = {name = name, weight = weight, children = children}
    end
    file:close()
    return programs
end

-- Helper function to split string
function string:split(sep)
    local result = {}
    for part in self:gmatch("[^" .. sep .. "]+") do
        table.insert(result, part)
    end
    return result
end

-- Function to calculate total weight of a subtree
local function calculate_weight(programs, name)
    local program = programs[name]
    local total_weight = program.weight
    for _, child in ipairs(program.children) do
        total_weight = total_weight + calculate_weight(programs, child)
    end
    program.total_weight = total_weight
    return total_weight
end

-- Function to find the unbalanced node and required adjustment
local function find_unbalanced(programs, name)
    local program = programs[name]
    if #program.children == 0 then return nil, 0 end

    local weights = {}
    for _, child in ipairs(program.children) do
        local unbalanced, adjustment = find_unbalanced(programs, child)
        if unbalanced then return unbalanced, adjustment end
        table.insert(weights, programs[child].total_weight)
    end

    table.sort(weights)
    if weights[1] ~= weights[#weights] then
        local different_weight = weights[1] == weights[2] and weights[#weights] or weights[1]
        local correct_weight = weights[1] == weights[2] and weights[1] or weights[#weights]
        for _, child in ipairs(program.children) do
            if programs[child].total_weight == different_weight then
                return child, correct_weight - different_weight
            end
        end
    end

    return nil, 0
end

-- Main function
local function main()
    local programs = read_input()
    local root = "dtacyn"  -- Use the result from Part One
    calculate_weight(programs, root)
    local unbalanced, adjustment = find_unbalanced(programs, root)
    if unbalanced then
        local correct_weight = programs[unbalanced].weight + adjustment
        print("Unbalanced program:", unbalanced)
        print("Correct weight:", correct_weight)
    else
        print("No unbalanced program found")
    end
end

main()
