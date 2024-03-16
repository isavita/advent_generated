using Base.Filesystem

function binaryToInt(binaryStr::String)
    result = 0
    for (i, char) in enumerate(binaryStr)
        if char == '1'
            result |= 1 << (length(binaryStr) - i)
        end
    end
    return result
end

function decode(pass::String)
    row = binaryToInt(pass[1:7])
    column = binaryToInt(pass[8:10])
    return row * 8 + column
end

seatIDs = []
open("input.txt", "r") do file
    for line in eachline(file)
        pass = replace(line, "F" => "0", "B" => "1", "L" => "0", "R" => "1")
        push!(seatIDs, decode(pass))
    end
end

sort!(seatIDs)

for i in 1:length(seatIDs)-1
    if seatIDs[i+1] != seatIDs[i] + 1
        println(seatIDs[i] + 1)
        break
    end
end