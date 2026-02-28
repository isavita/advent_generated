
#!/usr/bin/awk -f

function floorDiv(a,b,   q) {
    q = int(a/b)
    return (a < 0 && a % b != 0) ? q - 1 : q
}

BEGIN {
    dialSize = 100
    currentPos = 50
    totalZeroHits = 0

    while ((getline line < "input.txt") > 0) {
        gsub(/^[ \t]+|[ \t]+$/, "", line)
        if (line == "") continue
        dir = substr(line, 1, 1)
        amt = substr(line, 2) + 0

        if (dir == "R") {
            totalZeroHits += int((currentPos + amt) / dialSize)
            currentPos = (currentPos + amt) % dialSize
        } else if (dir == "L") {
            totalZeroHits += floorDiv(currentPos - 1, dialSize) - floorDiv(currentPos - amt - 1, dialSize)
            currentPos = (currentPos - amt) % dialSize
            if (currentPos < 0) currentPos += dialSize
        }
    }
    close("input.txt")
    print totalZeroHits
}
