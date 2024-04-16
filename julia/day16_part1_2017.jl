function spin(programs::Vector{Char}, x::Int)
    n = length(programs)
    return vcat(programs[(n-x+1):n], programs[1:(n-x)])
end

function exchange(programs::Vector{Char}, a::Int, b::Int)
    programs[a], programs[b] = programs[b], programs[a]
    return programs
end

function partner(programs::Vector{Char}, a::Char, b::Char)
    a_idx = findfirst(isequal(a), programs)
    b_idx = findfirst(isequal(b), programs)
    return exchange(programs, a_idx, b_idx)
end

function process_moves(programs::Vector{Char}, moves::Vector{String})
    for move in moves
        if startswith(move, "s")
            size = parse(Int, move[2:end])
            programs = spin(programs, size)
        elseif startswith(move, "x")
            a, b = parse.(Int, split(move[2:end], "/"))
            programs = exchange(programs, a + 1, b + 1)  # Adjust for 1-based indexing
        elseif startswith(move, "p")
            a, b = move[2], move[4]
            programs = partner(programs, a, b)
        end
    end
    return programs
end

function main()
    # Read input
    input = read("input.txt", String)
    moves = split(strip(input), ",")
    
    # Convert SubString elements to String
    moves = String.(moves)

    # Initial lineup of programs
    programs = collect('a':'p')
    
    # Process all moves
    final_programs = process_moves(programs, moves)
    
    # Print final order of programs
    println(join(final_programs))
end

main()