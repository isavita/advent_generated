
function transform(subjectNumber, loopSize)
    value = 1
    for i in 1:loopSize
        value *= subjectNumber
        value %= 20201227
    end
    return value
end

function findLoopSize(publicKey)
    value = 1
    loopSize = 0
    while value != publicKey
        value *= 7
        value %= 20201227
        loopSize += 1
    end
    return loopSize
end

file = open("input.txt")
cardPublicKey = parse(Int, readline(file))
doorPublicKey = parse(Int, readline(file))

cardLoopSize = findLoopSize(cardPublicKey)
encryptionKey = transform(doorPublicKey, cardLoopSize)

println(encryptionKey)
