
#!/usr/bin/awk -f

{
    split($0, parts, "-")
    start = parts[1]
    end = parts[2]

    count = 0
    for (i = start; i <= end; i++) {
        s = i
        if (hasDoubleAndIncreasingDigits(s)) {
            count++
        }
    }

    print count
}

function hasDoubleAndIncreasingDigits(s,   i) {
    hasDouble = 0
    for (i = 1; i < length(s); i++) {
        if (substr(s, i, 1) == substr(s, i+1, 1)) {
            hasDouble = 1
        }
        if (substr(s, i, 1) > substr(s, i+1, 1)) {
            return 0
        }
    }
    return hasDouble
}
