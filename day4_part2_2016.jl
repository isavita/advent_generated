using DelimitedFiles

function isRealRoom(room)
    parts = split(room, "[")
    checksum = strip(parts[2], ']')
    encryptedName = split(parts[1], "-")[1:end-1]

    letterCounts = Dict{Char, Int}()
    for part in encryptedName
        for letter in part
            letterCounts[letter] = get(letterCounts, letter, 0) + 1
        end
    end

    counts = [(letter, count) for (letter, count) in letterCounts]

    sort!(counts, by = x -> (-x[2], x[1]))

    for i in 1:length(checksum)
        if checksum[i] != Char(counts[i][1])
            return false
        end
    end

    return true
end

function getSectorID(room)
    parts = split(room, "-")
    sectorIDPart = split(parts[end], "[")[1]
    sectorID = parse(Int, sectorIDPart)
    return sectorID
end

function decryptName(room)
    parts = split(room, "-")
    sectorIDPart = split(parts[end], "[")[1]
    sectorID = parse(Int, sectorIDPart)
    decryptedName = ""

    for part in parts[1:end-1]
        for letter in part
            if letter == '-'
                decryptedName *= ' '
            else
                shiftedLetter = 'a' + mod(Int(letter - 'a') + sectorID, 26)
                decryptedName *= Char(shiftedLetter)
            end
        end
        decryptedName *= ' '
    end

    return strip(decryptedName)
end

input = readdlm("input.txt", String)
for room in input
    if isRealRoom(room)
        decryptedName = decryptName(room)
        if contains(decryptedName, "northpole object")
            println(getSectorID(room))
            break
        end
    end
end