function parse_input(filename)
    initial_state = ""
    rules = Dict()
    open(filename, "r") do file
        initial_state = match(r"initial state: ([#.]+)", readline(file)).captures[1]
        readline(file)  # skip the empty line
        for line in eachline(file)
            if occursin("=>", line)
                parts = split(line, " => ")
                pattern = parts[1]
                result = parts[2][1]  # Convert SubString to Char
                rules[pattern] = result
            end
        end
    end
    return initial_state, rules
end

function simulate_plants(initial_state, rules, generations)
    state = Dict{Int,Char}()
    for (i, plant) in enumerate(initial_state)
        state[i - 1] = plant  # Adjust index to start from 0
    end
    min_index, max_index = 0, length(initial_state) - 1

    for gen in 1:generations
        new_state = copy(state)
        for i in (min_index - 2):(max_index + 2)
            pattern = [get(state, k, '.') for k in (i-2):(i+2)]
            new_state[i] = get(rules, join(pattern), '.')
        end
        state = new_state
        min_index -= 2
        max_index += 2
    end

    return state
end

function sum_plant_indices(state)
    sum([idx for (idx, plant) in state if plant == '#'])
end

function main()
    initial_state, rules = parse_input("input.txt")
    final_state = simulate_plants(initial_state, rules, 20)
    result = sum_plant_indices(final_state)
    println("Sum of the numbers of all pots which contain a plant after 20 generations: $result")
end

main()