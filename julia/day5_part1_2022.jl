
function read_input(file_path)
    open(file_path, "r") do file
        return split(read(file, String), "\n\n")
    end
end

function parse_stacks(input)
    lines = split(input, "\n")
    num_stacks = div(length(lines[1]) + 1, 4)
    stacks = [Vector{Char}() for _ in 1:num_stacks]
    
    for line in lines
        for (i, char) in enumerate(line)
            if 'A' <= char <= 'Z'
                push!(stacks[div(i - 1, 4) + 1], char)
            end
        end
    end
    
    return [reverse(stack) for stack in stacks]
end

function parse_moves(steps)
    moves = []
    for step in split(steps, "\n")
        parts = split(step)
        n = parse(Int, parts[2])
        from = parse(Int, parts[4])
        to = parse(Int, parts[6])
        push!(moves, (n, from, to))
    end
    return moves
end

function move_crates(stacks, moves)
    for (n, from, to) in moves
        for _ in 1:n
            push!(stacks[to], pop!(stacks[from]))
        end
    end
    return [stack[end] for stack in stacks]
end

function main()
    input, steps = read_input("input.txt")
    stacks = parse_stacks(input)
    moves = parse_moves(steps)
    result = move_crates(stacks, moves)
    println(join(result))
end

main()
