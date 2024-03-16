using Printf

function binaryToInt(binaryStr::String)::Int
    result = 0
    for (i, char) in enumerate(binaryStr)
        if char == '1'
            result |= 1 << (length(binaryStr) - i)
        end
    end
    return result
end

function decode(pass::String)::Int
    row = binaryToInt(pass[1:7])
    column = binaryToInt(pass[8:end])
    return row * 8 + column
end

function main()
    maxSeatID = 0
    open("input.txt", "r") do file
        for line in eachline(file)
            pass = replace(line, "F" => "0", "B" => "1", "L" => "0", "R" => "1")
            seatID = decode(pass)
            if seatID > maxSeatID
                maxSeatID = seatID
            end
        end
    end
    @printf("%d\n", maxSeatID)
end

main()