using Combinatorics

function parse_input(filename)
    happiness_map = Dict()
    people = Set()
    open(filename, "r") do file
        for line in eachline(file)
            # Example line: "Alice would gain 54 happiness units by sitting next to Bob."
            m = match(r"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).", line)
            if m !== nothing
                person1, action, units, person2 = m.captures
                units = parse(Int, units) * (action == "gain" ? 1 : -1)
                happiness_map[(person1, person2)] = units
                push!(people, person1)
                push!(people, person2)
            end
        end
    end
    return happiness_map, collect(people)
end

function total_happiness(arrangement, happiness_map)
    n = length(arrangement)
    total = 0
    for i in 1:n
        left = arrangement[i]
        right = arrangement[mod1(i+1, n)]
        total += get(happiness_map, (left, right), 0) + get(happiness_map, (right, left), 0)
    end
    return total
end

function maximum_happiness(people, happiness_map)
    max_happiness = -Inf
    for perm in permutations(people)
        happiness = total_happiness(perm, happiness_map)
        if happiness > max_happiness
            max_happiness = happiness
        end
    end
    return max_happiness
end

function main()
    happiness_map, people = parse_input("input.txt")
    max_happy_without_me = maximum_happiness(people, happiness_map)
    println("Maximum happiness without me: ", max_happy_without_me)
    
    # Part Two: Add yourself to the table
    for person in people
        happiness_map[("Me", person)] = 0
        happiness_map[(person, "Me")] = 0
    end
    push!(people, "Me")
    
    max_happy_with_me = maximum_happiness(people, happiness_map)
    println("Maximum happiness with me: ", max_happy_with_me)
end

main()