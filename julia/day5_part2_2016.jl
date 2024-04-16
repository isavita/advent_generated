using MD5

function findPassword(doorID)
    password = Vector{Char}(undef, 8)
    filledPositions = 0
    found = falses(8)

    i = 0
    while filledPositions < 8
        hash = bytes2hex(md5(doorID * string(i)))
        if startswith(hash, "00000")
            pos = hash[6]
            if '0' <= pos <= '7'
                posIndex = parse(Int, pos) + 1
                if !found[posIndex]
                    found[posIndex] = true
                    password[posIndex] = hash[7]
                    filledPositions += 1
                end
            end
        end
        i += 1
    end
    return join(password)
end

doorID = strip(read("input.txt", String))
password = findPassword(doorID)
println(password)