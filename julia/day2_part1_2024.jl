
function solve()
    safe_report_count = 0
    for line in eachline("input.txt")
        levels = parse.(Int, split(line))
        if is_safe_report(levels)
            safe_report_count += 1
        end
    end
    println(safe_report_count)
end

function is_safe_report(levels::Vector{Int})
    length(levels) < 2 && return false
    first_diff = levels[2] - levels[1]
    first_diff == 0 && return false
    is_increasing = first_diff > 0
    @inbounds for i in 1:length(levels)-1
        diff = levels[i+1] - levels[i]
        diff == 0 && return false
        (is_increasing && diff <= 0) && return false
        (!is_increasing && diff >= 0) && return false
        abs_diff = abs(diff)
        (abs_diff < 1 || abs_diff > 3) && return false
    end
    return true
end

solve()
