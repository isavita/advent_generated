
#!/usr/bin/env bash
# Find the 2nd next valid password for the given rules.
# Input is read from input.txt, output is printed to stdout.

awk '
# Convert a character to its ASCII code (a‑z only)
function ord(c) { return index("abcdefghijklmnopqrstuvwxyz", c) + 96 }

# Convert an ASCII code to a character
function chr(n) { return sprintf("%c", n) }

# Does the string contain a straight of three increasing letters?
function hasStraight(s) {
    for (i = 1; i + 2 <= length(s); i++) {
        c1 = substr(s, i, 1)
        c2 = substr(s, i + 1, 1)
        c3 = substr(s, i + 2, 1)
        if (ord(c1) + 1 == ord(c2) && ord(c1) + 2 == ord(c3))
            return 1
    }
    return 0
}

# Does the string contain any of i, o, l ?
function containsInvalidLetters(s) { return s ~ /[iol]/ }

# Does the string contain at least two different non‑overlapping pairs ?
function hasTwoPairs(s) {
    n = 0
    i = 1
    while (i <= length(s) - 1) {
        if (substr(s, i, 1) == substr(s, i + 1, 1)) {
            n++
            i += 2
        } else
            i++
    }
    return n >= 2
}

# Is the password valid ?
function isValidPassword(s) {
    return hasStraight(s) && !containsInvalidLetters(s) && hasTwoPairs(s)
}

# Increment the password by one, skipping i, o, l
function incrementPassword(s,    i, c, newc) {
    for (i = length(s); i >= 1; i--) {
        c = substr(s, i, 1)
        if (c == "z") {
            s = substr(s, 1, i - 1) "a" substr(s, i + 1)
        } else {
            newc = chr(ord(c) + 1)
            if (newc == "i" || newc == "o" || newc == "l")
                newc = chr(ord(newc) + 1)
            s = substr(s, 1, i - 1) newc substr(s, i + 1)
            break
        }
    }
    return s
}

# Main
{
    pwd = $1
    for (cnt = 0; cnt < 2; cnt++) {
        do {
            pwd = incrementPassword(pwd)
        } while (!isValidPassword(pwd))
    }
    print pwd
}
' input.txt
