function is_nice(str::String)::Bool
    # Check for at least three vowels
    vowels_count = count(c -> c in "aeiou", str)
    if vowels_count < 3
        return false
    end

    # Check for at least one letter that appears twice in a row
    has_double = any([str[i] == str[i+1] for i in 1:length(str)-1])
    if !has_double
        return false
    end

    # Check for disallowed substrings
    for bad in ["ab", "cd", "pq", "xy"]
        if occursin(bad, str)
            return false
        end
    end

    return true
end

function count_nice_strings(filename::String)::Int
    nice_count = 0
    open(filename, "r") do file
        for line in eachline(file)
            if is_nice(line)
                nice_count += 1
            end
        end
    end
    return nice_count
end

# Assuming the input file is named "input.txt"
println(count_nice_strings("input.txt"))
