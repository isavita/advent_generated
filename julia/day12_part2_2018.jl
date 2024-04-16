function main()
    initial_state = ""
    rules = Dict{String,Char}()

    open("input.txt") do file
        for line in eachline(file)
            if contains(line, "initial state")
                initial_state = split(line, ": ")[2]
            elseif contains(line, "=>")
                parts = split(line, " => ")
                rules[parts[1]] = parts[2][1]
            end
        end
    end

    state = Dict{Int,Char}()
    for (i, c) in enumerate(initial_state)
        if c == '#'
            state[i-1] = '#'
        end
    end

    previous_pattern = ""
    previous_sum = 0
    offset = 0
    for generation in 0:49999999999
        new_state = Dict{Int,Char}()
        min_pot, max_pot = extrema(keys(state))
        for i in min_pot-2:max_pot+2
            pattern = join([get(state, j, '.') for j in i-2:i+2])
            if get(rules, pattern, '.') == '#'
                new_state[i] = '#'
            end
        end
        state = new_state

        current_pattern, current_sum = state_pattern(state)
        if current_pattern == previous_pattern
            offset = current_sum - previous_sum
            remaining_generations = 49999999999 - generation
            final_sum = current_sum + offset * remaining_generations
            println(final_sum)
            return
        end
        previous_pattern = current_pattern
        previous_sum = current_sum
    end
end

function state_pattern(state)
    min_pot, max_pot = extrema(keys(state))
    pattern = join([get(state, i, '.') for i in min_pot:max_pot])
    sum = 0
    for (i, c) in enumerate(pattern)
        if c == '#'
            sum += i + min_pot - 1
        end
    end
    return pattern, sum
end

main()