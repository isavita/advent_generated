
function sortString(w)
    s = split(w, "")
    sort!(s)
    return join(s, "")
end

data = read("input.txt", String)
passphrases = split(strip(data), "\n")
validCount = 0

for passphrase in passphrases
    words = split(passphrase)
    wordSet = Dict()

    valid = true
    for word in words
        sortedWord = sortString(word)
        if haskey(wordSet, sortedWord)
            valid = false
            break
        end
        wordSet[sortedWord] = true
    end

    if valid
        global validCount += 1
    end
end

println(validCount)
