
{
    totalDiff = 0
    while (getline < "input.txt") {
        codeLength = length($0)
        memoryLength = calculateMemoryLength($0)
        totalDiff += codeLength - memoryLength
    }
    print totalDiff
}

function calculateMemoryLength(s) {
    len = 0
    inEscape = 0
    hexCount = 0

    for (i = 2; i <= length(s)-1; i++) {
        if (hexCount > 0) {
            hexCount--
        } else if (inEscape == 1) {
            if (substr(s, i, 1) == "x") {
                hexCount = 2
            }
            inEscape = 0
            len++
        } else if (substr(s, i, 1) == "\\") {
            inEscape = 1
        } else {
            len++
        }
    }
    return len
}
