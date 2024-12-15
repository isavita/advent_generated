
function solve()
    safe_count = 0
    for line in eachline("input.txt")
        levels = parse.(Int, split(line))
        if is_safe_report(levels) || is_safe_with_one_removal(levels)
            safe_count += 1
        end
    end
    println(safe_count)
end

function is_safe_report(levels)
    length(levels) < 2 && return false
    first_diff = levels[2] - levels[1]
    first_diff == 0 && return false
    is_increasing = first_diff > 0
    for i in 1:length(levels)-1
        diff = levels[i+1] - levels[i]
        diff == 0 && return false
        (is_increasing && diff <= 0) && return false
        (!is_increasing && diff >= 0) && return false
        abs(diff) < 1 && return false
        abs(diff) > 3 && return false
    end
    return true
end

function is_safe_with_one_removal(levels)
    for i in 1:length(levels)
        modified_levels = levels[1:end .!= i]
        if is_safe_report(modified_levels)
            return true
        end
    end
    return false
end

solve()
