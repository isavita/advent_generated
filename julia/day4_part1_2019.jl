
function hasDoubleAndIncreasingDigits(s)
    hasDouble = false
    for i in 1:length(s)-1
        if s[i] == s[i+1]
            hasDouble = true
        end
        if s[i] > s[i+1]
            return false
        end
    end
    return hasDouble
end

open("input.txt") do file
    rangeStr = readline(file)
    parts = split(rangeStr, "-")
    start = parse(Int, parts[1])
    stop = parse(Int, parts[2])

    count = 0
    for i in start:stop
        s = string(i)
        if hasDoubleAndIncreasingDigits(s)
            count += 1
        end
    end

    println(count)
end
