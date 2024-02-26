
#!/usr/bin/awk -f

BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, parts, " ")
        id = substr(parts[1], 2)
        split(substr(parts[3], 1, length(parts[3])-1), coords, ",")
        x = coords[1]
        y = coords[2]
        split(parts[4], dims, "x")
        width = dims[1]
        height = dims[2]
        claims[id] = x " " y " " width " " height
    }
    close("input.txt")

    for (i = 0; i < 1000; i++) {
        for (j = 0; j < 1000; j++) {
            fabric[i, j] = 0
        }
    }

    for (claim in claims) {
        split(claims[claim], parts, " ")
        x = parts[1]
        y = parts[2]
        width = parts[3]
        height = parts[4]
        for (k = y; k < y + height; k++) {
            for (l = x; l < x + width; l++) {
                fabric[k, l]++
            }
        }
    }

    for (claim in claims) {
        split(claims[claim], parts, " ")
        x = parts[1]
        y = parts[2]
        width = parts[3]
        height = parts[4]
        overlap = 0
        for (k = y; k < y + height; k++) {
            for (l = x; l < x + width; l++) {
                if (fabric[k, l] > 1) {
                    overlap = 1
                    break
                }
            }
            if (overlap == 1) {
                break
            }
        }
        if (overlap == 0) {
            print claim
            exit
        }
    }
}
