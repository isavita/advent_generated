
function parse_input(filename)
    lines = readlines(filename)
    instructions = collect(lines[1])
    
    network = Dict{String, Tuple{String, String}}()
    for line in lines[3:end]
        node, paths = split(line, " = ")
        left, right = match(r"\((\w+), (\w+)\)", paths).captures
        network[node] = (left, right)
    end
    
    return instructions, network
end

function solve_part1(instructions, network)
    current = "AAA"
    steps = 0
    instruction_length = length(instructions)
    
    while current != "ZZZ"
        instruction = instructions[(steps % instruction_length) + 1]
        current = instruction == 'L' ? network[current][1] : network[current][2]
        steps += 1
    end
    
    return steps
end

function solve_part2(instructions, network)
    start_nodes = [node for node in keys(network) if endswith(node, "A")]
    
    # Find cycle length for each starting node
    cycle_lengths = map(start_nodes) do start_node
        current = start_node
        steps = 0
        instruction_length = length(instructions)
        
        while !endswith(current, "Z")
            instruction = instructions[(steps % instruction_length) + 1]
            current = instruction == 'L' ? network[current][1] : network[current][2]
            steps += 1
        end
        
        return steps
    end
    
    # Compute least common multiple of cycle lengths
    return lcm(cycle_lengths...)
end

function main()
    instructions, network = parse_input("input.txt")
    
    part1_result = solve_part1(instructions, network)
    println("Part 1: ", part1_result)
    
    part2_result = solve_part2(instructions, network)
    println("Part 2: ", part2_result)
end

main()
