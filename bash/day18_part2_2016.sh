
#!/usr/bin/env bash
# -------------  safe-tiles.sh  -------------
# Reads the first line of input.txt and counts the safe tiles
# after generating 400 000 rows according to the trap rules.

TOTAL_ROWS=400000

awk -v totalRows="$TOTAL_ROWS" '
# Read the first (and only) line of the file
NR==1 {
    row = $0
    len = length(row)

    # Count safe tiles in the first row
    safeCount = 0
    for (i = 1; i <= len; i++) {
        if (substr(row, i, 1) == ".") safeCount++
    }
    next          # skip any other lines
}

# After the file is read, generate the remaining rows
END {
    for (r = 1; r < totalRows; r++) {
        nextRow = ""
        for (j = 1; j <= len; j++) {
            left   = (j == 1   ? "." : substr(row, j-1, 1))
            center = substr(row, j, 1)
            right  = (j == len ? "." : substr(row, j+1, 1))

            if ((left   == "^" && center == "^" && right  == ".") ||
                (center == "^" && right  == "^" && left   == ".") ||
                (left   == "^" && center == "." && right  == ".") ||
                (right  == "^" && center == "." && left   == ".")) {
                nextRow = nextRow "^"
            } else {
                nextRow = nextRow "."
                safeCount++
            }
        }
        row = nextRow
    }
    print safeCount
}
' input.txt
