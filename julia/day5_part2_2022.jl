
function solve()
    input = read("input.txt", String)
    sections = split(input, "\n\n")
    stack_lines = split(sections[1], "\n")
    
    num_stacks = (length(stack_lines[1]) + 1) ÷ 4
    stacks = [Char[] for _ in 1:num_stacks]
    
    for line in stack_lines
        for (i, c) in enumerate(line)
            if 'A' <= c <= 'Z'
                pushfirst!(stacks[(i-1)÷4 + 1], c)
            end
        end
    end
    
    steps = split(sections[2], "\n")
    
    for step in steps
        n, from, to = parse.(Int, match(r"move (\d+) from (\d+) to (\d+)", step).captures)
        moved = stacks[from][end-n+1:end]
        deleteat!(stacks[from], length(stacks[from])-n+1:length(stacks[from]))
        append!(stacks[to], moved)
    end
    
    return join(last.(stacks))
end

println(solve())
