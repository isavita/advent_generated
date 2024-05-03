BEGIN {
    getline password < "input.txt"
    while (1) {
        password = incrementPassword(password)
        if (isValidPassword(password)) {
            print password
            exit
        }
    }
}

function incrementPassword(password,      i, runes) {
    split(password, runes, "")
    for (i = length(runes); i >= 1; i--) {
        if (runes[i] == "z") {
            runes[i] = "a"
        } else {
            runes[i] = sprintf("%c", index("abcdefghijklmnopqrstuvwxyz", runes[i]) + 1 + 96)
            break
        }
    }
    return join(runes, "")
}

function isValidPassword(password,      i) {
    if (!hasStraight(password)) return 0
    if (containsInvalidLetters(password)) return 0
    if (!hasTwoPairs(password)) return 0
    return 1
}

function hasStraight(password,      i) {
    for (i = 1; i < length(password) - 2; i++) {
        if (substr(password, i, 1) == chr(ascii(substr(password, i + 1, 1)) - 1) && 
            substr(password, i, 1) == chr(ascii(substr(password, i + 2, 1)) - 2)) {
            return 1
        }
    }
    return 0
}

function containsInvalidLetters(password,      i) {
    for (i = 1; i <= length(password); i++) {
        if (substr(password, i, 1) ~ /[iol]/) return 1
    }
    return 0
}

function hasTwoPairs(password,      i, count) {
    count = 0
    for (i = 1; i < length(password); i++) {
        if (substr(password, i, 1) == substr(password, i + 1, 1)) {
            count++
            i++
        }
    }
    return count >= 2
}

function ascii(c) {
    return index("abcdefghijklmnopqrstuvwxyz", tolower(c))
}

function chr(n) {
    return sprintf("%c", n + 96)
}

function join(a,      s, i) {
    s = a[1]
    for (i = 2; i <= length(a); i++) {
        s = s a[i]
    }
    return s
}