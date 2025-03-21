
function readInput(filename)
    open(filename) do file
        return readline(file)
    end
end

function main()
    currentPassword = readInput("input.txt")
    firstNewPassword = findNextPassword(currentPassword)
    secondNewPassword = findNextPassword(firstNewPassword)
    println(secondNewPassword)
end

function findNextPassword(password)
    while true
        password = incrementPassword(password)
        if isValidPassword(password)
            break
        end
    end
    return password
end

function incrementPassword(password)
    password = collect(password)  # Convert string to array of characters
    for i in length(password):-1:1
        password[i] += 1
        if password[i] > 'z'
            password[i] = 'a'
        else
            break
        end
    end
    return join(password)  # Convert array of characters back to string
end

function isValidPassword(password)
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password)
end

function hasStraight(password)
    for i in 1:length(password)-2
        if password[i] + 1 == password[i+1] && password[i] + 2 == password[i+2]
            return true
        end
    end
    return false
end

function containsInvalidLetters(password)
    for c in password
        if c == 'i' || c == 'o' || c == 'l'
            return true
        end
    end
    return false
end

function hasTwoPairs(password)
    count = 0
    i = 1
    while i < length(password)
        if password[i] == password[i+1]
            count += 1
            i += 2
        else
            i += 1
        end
    end
    return count >= 2
end

main()
