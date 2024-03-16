function calculateEncodedLength(s::String)::Int
    encoded = "\""
    for c in s
        if c == '\\' || c == '"'
            encoded *= "\\"
        end
        encoded *= string(c)
    end
    encoded *= "\""
    return length(encoded)
end

open("input.txt", "r") do file
    totalDiff = 0
    for line in eachline(file)
        originalLength = length(line)
        encodedLength = calculateEncodedLength(line)
        totalDiff += encodedLength - originalLength
    end
    println(totalDiff)
end