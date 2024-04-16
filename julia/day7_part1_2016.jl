function contains_abba(s)
    for i in 1:length(s)-3
        if s[i] == s[i+3] && s[i+1] == s[i+2] && s[i] != s[i+1]
            return true
        end
    end
    return false
end

function supports_tls(line)
    # Split the line into parts inside and outside of brackets
    outside_brackets = []
    inside_brackets = []
    parts = split(line, ['[', ']'])
    for i in 1:length(parts)
        if i % 2 == 1
            push!(outside_brackets, parts[i])
        else
            push!(inside_brackets, parts[i])
        end
    end

    # Check for ABBA outside brackets and no ABBA inside brackets
    has_abba_outside = any(contains_abba.(outside_brackets))
    has_abba_inside = any(contains_abba.(inside_brackets))

    return has_abba_outside && !has_abba_inside
end

function count_tls_supporting_ips(filename)
    count = 0
    open(filename, "r") do file
        for line in eachline(file)
            if supports_tls(strip(line))
                count += 1
            end
        end
    end
    return count
end

# Read from "input.txt" and print the number of IPs that support TLS
println(count_tls_supporting_ips("input.txt"))