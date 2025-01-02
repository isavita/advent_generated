
function solve(input)
    rows = map(input) do line
        parts = split(line, " ")
        springs = parts[1]
        group = parse.(Int, split(parts[2], ","))
        (springs, group)
    end

    function unfold_row(row, unfolding_factor)
        springs, group = row
        new_springs = springs
        new_group = group
        for _ in 2:unfolding_factor
            new_springs *= "?" * springs
            new_group = vcat(new_group, group)
        end
        (new_springs, new_group)
    end

    unfolded_rows = map(row -> unfold_row(row, 5), rows)

    function count_arrangements_recursive(row, i_springs, i_group, i_contiguous_damaged, cache)
        springs, group = row
        if i_springs > length(springs)
            if i_group > length(group) && i_contiguous_damaged == 0
                return 1
            elseif i_group == length(group) && i_contiguous_damaged == group[i_group]
                return 1
            end
            return 0
        end

        cache_key = (i_springs, i_group, i_contiguous_damaged)
        if haskey(cache, cache_key)
            return cache[cache_key]
        end

        res = 0
        char = springs[i_springs]
        if char == '.' || char == '?'
            if i_contiguous_damaged == 0
                res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged, cache)
            elseif i_group <= length(group) && i_contiguous_damaged == group[i_group]
                res += count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, cache)
            end
        end
        if char == '#' || char == '?'
            if i_group <= length(group) && i_contiguous_damaged < get(group, i_group, 0)
                res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged + 1, cache)
            end
        end

        cache[cache_key] = res
        return res
    end

    function count_arrangements(row)
        cache = Dict()
        count_arrangements_recursive(row, 1, 1, 0, cache)
    end

    res = 0
    for row in unfolded_rows
        res += count_arrangements(row)
    end

    return res
end

input = readlines("input.txt")
println(solve(input))
