using Combinatorics

# Function to parse the input file and build the happiness map
function parse_happiness_map(filename)
    happiness_map = Dict()
    open(filename, "r") do file
        for line in eachline(file)
            # Example line: "Alice would gain 54 happiness units by sitting next to Bob."
            m = match(r"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).", line)
            if m !== nothing
                person1, action, units, person2 = m.captures
                units = parse(Int, units)
                units = action == "lose" ? -units : units
                happiness_map[(person1, person2)] = units
            end
        end
    end
    return happiness_map
end

# Function to calculate total happiness for a given seating arrangement
function calculate_happiness(arrangement, happiness_map)
    n = length(arrangement)
    total_happiness = 0
    for i in 1:n
        left = arrangement[i]
        right = arrangement[mod1(i+1, n)]  # mod1 for circular indexing
        total_happiness += get(happiness_map, (left, right), 0)
        total_happiness += get(happiness_map, (right, left), 0)
    end
    return total_happiness
end

# Main function to find the optimal seating arrangement
function optimal_seating_happiness(filename)
    happiness_map = parse_happiness_map(filename)
    guests = unique([pair[1] for pair in keys(happiness_map)])
    all_arrangements = permutations(guests)
    max_happiness = -Inf
    for arrangement in all_arrangements
        happiness = calculate_happiness(arrangement, happiness_map)
        max_happiness = max(max_happiness, happiness)
    end
    return max_happiness
end

# Read the file and compute the result
filename = "input.txt"
result = optimal_seating_happiness(filename)
println("The total change in happiness for the optimal seating arrangement is: $result")