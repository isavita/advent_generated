
function parse_input(input)
    rows = []
    for line in input
        parts = split(line, " ")
        springs = parts[1]
        ints = parse.(Int, split(parts[2], ","))
        push!(rows, (springs, ints))
    end
    return rows
end

function count_arrangements_recursive(springs, groups, i_springs, i_group, i_contiguous_damaged, cache)
    if i_springs > length(springs)
        if i_group > length(groups) && i_contiguous_damaged == 0
            return 1
        elseif i_group == length(groups) && i_contiguous_damaged == groups[i_group]
            return 1
        else
            return 0
        end
    end

    key = (i_springs, i_group, i_contiguous_damaged)
    if haskey(cache, key)
        return cache[key]
    end

    res = 0
    char = springs[i_springs]
    if char == '.' || char == '?'
        if i_contiguous_damaged == 0
            res += count_arrangements_recursive(springs, groups, i_springs + 1, i_group, i_contiguous_damaged, cache)
        elseif i_group <= length(groups) && i_contiguous_damaged == groups[i_group]
            res += count_arrangements_recursive(springs, groups, i_springs + 1, i_group + 1, 0, cache)
        end
    end
    if char == '#' || char == '?'
        if i_group <= length(groups) && i_contiguous_damaged < groups[i_group]
            res += count_arrangements_recursive(springs, groups, i_springs + 1, i_group, i_contiguous_damaged + 1, cache)
        end
    end

    cache[key] = res
    return res
end

function count_arrangements(springs, groups)
    return count_arrangements_recursive(springs, groups, 1, 1, 0, Dict{Tuple{Int,Int,Int},Int}())
end

function unfold_row(springs, groups, unfolding_factor)
    new_springs = springs
    new_groups = groups
    for _ in 2:unfolding_factor
        new_springs *= "?" * springs
        append!(new_groups, groups)
    end
    return new_springs, new_groups
end

function solve(input)
    rows = parse_input(input)
    res = 0
    for (springs, groups) in rows
        res += count_arrangements(springs, groups)
    end
    return res
end

input = readlines("input.txt")
println(solve(input))
