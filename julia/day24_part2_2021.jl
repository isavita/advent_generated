
function solve()
    lines = readlines("input.txt")
    k = Int[]
    l = Int[]
    m = Int[]

    for (i, line) in enumerate(lines)
        if (i-1) % 18 == 4
            push!(l, parse(Int, split(line)[3]))
        elseif (i-1) % 18 == 5
            push!(k, parse(Int, split(line)[3]))
        elseif (i-1) % 18 == 15
            push!(m, parse(Int, split(line)[3]))
        end
    end

    constraints = Dict{Int, Tuple{Int, Int}}()
    stack = Int[]

    for i in eachindex(l)
        if l[i] == 1
            push!(stack, i)
        elseif l[i] == 26
            pop = pop!(stack)
            constraints[pop] = (i, m[pop] + k[i])
        end
    end

    min_digits = zeros(Int, 14)

    for i in eachindex(l)
        if !haskey(constraints, i)
            continue
        end

        vmin = 1
        while vmin + constraints[i][2] < 1
            vmin += 1
        end

        min_digits[i] = vmin
        min_digits[constraints[i][1]] = vmin + constraints[i][2]
    end

    result = foldl((acc, x) -> acc * 10 + x, min_digits)
    println(result)
end

solve()
