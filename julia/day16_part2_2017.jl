function parse_move(move)
    if move[1] == 's'
        return ('s', parse(Int, move[2:end]))
    elseif move[1] == 'x'
        positions = split(move[2:end], '/')
        return ('x', parse(Int, positions[1]), parse(Int, positions[2]))
    elseif move[1] == 'p'
        partners = split(move[2:end], '/')
        return ('p', partners[1][1], partners[2][1])
    end
end

function perform_move!(programs, move)
    if move[1] == 's'
        spin_size = move[2]
        programs[:] = vcat(programs[end-spin_size+1:end], programs[1:end-spin_size])
    elseif move[1] == 'x'
        programs[move[2]+1], programs[move[3]+1] = programs[move[3]+1], programs[move[2]+1]
    elseif move[1] == 'p'
        idx1 = findfirst(==(move[2]), programs)
        idx2 = findfirst(==(move[3]), programs)
        programs[idx1], programs[idx2] = programs[idx2], programs[idx1]
    end
end

function dance(programs, moves)
    for move in moves
        perform_move!(programs, move)
    end
    return programs
end

function repeated_dance(programs, moves, num_times)
    seen_states = Dict(join(programs) => 0)
    for i in 1:num_times
        dance(programs, moves)
        state = join(programs)
        if haskey(seen_states, state)
            cycle_length = i - seen_states[state]
            remaining_dances = (num_times - i) % cycle_length
            return repeated_dance(programs, moves, remaining_dances)
        else
            seen_states[state] = i
        end
    end
    return programs
end

function main()
    input = read("input.txt", String)
    moves = [parse_move(move) for move in split(strip(input), ",")]

    # Part 1
    programs = collect('a':'p')
    result_part1 = dance(copy(programs), moves)
    println("Part 1: ", join(result_part1))

    # Part 2
    result_part2 = repeated_dance(programs, moves, 1000000000)
    println("Part 2: ", join(result_part2))
end

main()