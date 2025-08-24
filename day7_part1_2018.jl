function parse_input(filename)
    deps = Dict{Char,Vector{Char}}()
    all_steps = Set{Char}()

    open(filename) do file
        for line in eachline(file)
            m = match(r"Step (\w) must be finished before step (\w) can begin.", line)
            a, b = m.captures
            if !haskey(deps, b[1])
                deps[b[1]] = Char[]
            end
            push!(deps[b[1]], a[1])
            push!(all_steps, a[1])
            push!(all_steps, b[1])
        end
    end

    return deps, all_steps
end

function topological_sort(deps, all_steps)
    order = Char[]
    available = Char[]

    # Find initial available steps (with no dependencies)
    for step in all_steps
        if !haskey(deps, step) || isempty(deps[step])
            push!(available, step)
        end
    end
    sort!(available)

    while !isempty(available)
        # Pick the step that comes first alphabetically
        next = popfirst!(available)
        push!(order, next)

        # Check and update the availability of the next steps
        for step in all_steps
            if haskey(deps, step) && next in deps[step]
                deleteat!(deps[step], findfirst(isequal(next), deps[step]))
                if isempty(deps[step])
                    push!(available, step)
                end
            end
        end
        sort!(available)
    end

    return join(order)
end

# Read and parse the input file
deps, all_steps = parse_input("input.txt")

# Compute the order of steps
order = topological_sort(deps, all_steps)

println(order)