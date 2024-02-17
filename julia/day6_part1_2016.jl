
messages = readlines("input.txt")

function getCorrectedMessage(messages)
    if length(messages) == 0
        return ""
    end
    messageLength = length(messages[1])
    count = [Dict{Char, Int}() for _ in 1:messageLength]

    for message in messages
        for (j, char) in enumerate(message)
            count[j][char] = get(count[j], char, 0) + 1
        end
    end

    correctedMessage = ""
    for charCount in count
        correctedMessage *= getMostCommonChar(charCount)
    end

    return correctedMessage
end

function getMostCommonChar(count)
    maxChar = ' '
    maxCount = 0
    for (char, cnt) in count
        if cnt > maxCount
            maxCount = cnt
            maxChar = char
        end
    end
    return maxChar
end

correctedMessage = getCorrectedMessage(messages)
println(correctedMessage)
