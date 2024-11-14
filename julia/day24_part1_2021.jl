
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

    max_num = zeros(Int, 14)
    for i in 1:14
        if !haskey(constraints, i)
            continue
        end
        vmax = 9
        while vmax + constraints[i][2] > 9
            vmax -= 1
        end
        max_num[i] = vmax
        max_num[constraints[i][1]] = vmax + constraints[i][2]
    end

    result = 0
    for digit in max_num
        result = result * 10 + digit
    end

    return result
end

println(solve())
