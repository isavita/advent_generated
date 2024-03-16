function decompressed_length_v1(s)
    length(s) == 0 && return 0
    r = 0
    i = 1
    while i <= length(s)
        if s[i] == '('
            j = i + 1
            while s[j] != 'x'
                j += 1
            end
            marker_length = parse(Int, s[i+1:j-1])
            k = j + 1
            while s[k] != ')'
                k += 1
            end
            repetitions = parse(Int, s[j+1:k-1])
            i = k + 1
            r += marker_length * repetitions
        else
            r += 1
            i += 1
        end
    end
    return r
end

function decompressed_length_v2(s)
    length(s) == 0 && return 0
    r = 0
    i = 1
    while i <= length(s)
        if s[i] == '('
            j = i + 1
            while s[j] != 'x'
                j += 1
            end
            marker_length = parse(Int, s[i+1:j-1])
            k = j + 1
            while s[k] != ')'
                k += 1
            end
            repetitions = parse(Int, s[j+1:k-1])
            i = k + 1
            marker_string = s[i:i+marker_length-1]
            r += decompressed_length_v2(marker_string) * repetitions
            i += marker_length
        else
            r += 1
            i += 1
        end
    end
    return r
end

input = readline("input.txt")
input = replace(input, r"\s+" => "")
println(decompressed_length_v1(input))
println(decompressed_length_v2(input))