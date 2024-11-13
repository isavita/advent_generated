
local function read_input()
    local file = io.open("input.txt", "r")
    if not file then error("Could not open input file") end
    
    local instructions = file:read("*line")
    file:read("*line")  -- Skip blank line
    
    local network = {}
    for line in file:lines() do
        local node, left, right = line:match("(%w+) = %((%w+), (%w+)%)")
        network[node] = {left = left, right = right}
    end
    
    file:close()
    return instructions, network
end

local function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

local function lcm(a, b)
    return (a * b) / gcd(a, b)
end

local function solve_part1(instructions, network)
    local current = "AAA"
    local steps = 0
    local instruction_length = #instructions
    
    while current ~= "ZZZ" do
        local instruction = instructions:sub((steps % instruction_length) + 1, (steps % instruction_length) + 1)
        current = instruction == "L" and network[current].left or network[current].right
        steps = steps + 1
    end
    
    return steps
end

local function solve_part2(instructions, network)
    local start_nodes = {}
    for node in pairs(network) do
        if node:sub(-1) == "A" then
            table.insert(start_nodes, node)
        end
    end
    
    local cycle_lengths = {}
    for _, start_node in ipairs(start_nodes) do
        local current = start_node
        local steps = 0
        local instruction_length = #instructions
        
        while current:sub(-1) ~= "Z" do
            local instruction = instructions:sub((steps % instruction_length) + 1, (steps % instruction_length) + 1)
            current = instruction == "L" and network[current].left or network[current].right
            steps = steps + 1
        end
        
        table.insert(cycle_lengths, steps)
    end
    
    local result = cycle_lengths[1]
    for i = 2, #cycle_lengths do
        result = lcm(result, cycle_lengths[i])
    end
    
    return result
end

local function main()
    local instructions, network = read_input()
    
    local part1_result = solve_part1(instructions, network)
    print("Part 1 Result:", part1_result)
    
    local part2_result = solve_part2(instructions, network)
    print("Part 2 Result:", part2_result)
end

main()
