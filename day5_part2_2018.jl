# Function to react the polymer based on the given rules
function react_polymer(polymer)
    stack = Char[]
    for unit in polymer
        if !isempty(stack) && abs(Int(unit) - Int(last(stack))) == 32
            pop!(stack)
        else
            push!(stack, unit)
        end
    end
    return stack
end

# Function to remove all instances of a unit (case insensitive) and react the polymer
function remove_and_react(polymer, unit)
    filtered_polymer = filter(c -> lowercase(c) != lowercase(unit), polymer)
    return react_polymer(filtered_polymer)
end

# Read the polymer from the input file
input_file = "input.txt"
polymer = read(input_file, String)

# Part 1: React the polymer and find the length
reacted_polymer = react_polymer(polymer)
println("Part 1 - Remaining units after full reaction: ", length(reacted_polymer))

# Part 2: Find the shortest polymer by removing each unit type
unit_types = unique(lowercase(polymer))
shortest_length = length(polymer)  # Start with the original length as the upper bound

for unit in unit_types
    global shortest_length  # Explicitly declare that we are modifying the global variable
    test_polymer = remove_and_react(polymer, unit)
    shortest_length = min(shortest_length, length(test_polymer))
end

println("Part 2 - Shortest possible polymer length: ", shortest_length)