
#!/usr/bin/awk -f

function has_adjacent(num,    s, n, i) {
    s = num ""
    n = length(s)
    for (i = 1; i < n; i++) {
        if (substr(s, i, 1) == substr(s, i+1, 1)) {
            return 1
        }
    }
    return 0
}

function has_only_two_adjacent(num,    s, n, i, count) {
    s = num ""
    n = length(s)
    count = 1
    for (i = 1; i < n; i++) {
        if (substr(s, i, 1) == substr(s, i+1, 1)) {
            count++
        } else {
            if (count == 2) {
                return 1
            }
            count = 1
        }
    }
    if (count == 2) {
        return 1
    }
    return 0
}

function never_decreases(num,    s, n, i) {
    s = num ""
    n = length(s)
    for (i = 1; i < n; i++) {
        if (substr(s, i, 1) > substr(s, i+1, 1)) {
            return 0
        }
    }
    return 1
}

# 'main' entry point starts here in the BEGIN block
BEGIN {
    if (getline < "input.txt" > 0) {
        split($0, range, "-")
        start = range[1]
        end = range[2]
    } else {
        print "Error reading input.txt" > "/dev/stderr"
        exit 1
    }

    count_part1 = 0
    count_part2 = 0

    for (i = start; i <= end; i++) {
        nd = never_decreases(i)
        if (nd) {
             if (has_adjacent(i)) {
                count_part1++
             }
             if (has_only_two_adjacent(i)) {
                 count_part2++
             }
        }
    }

    print count_part1
    print count_part2
}
