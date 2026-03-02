
BEGIN {
    while ("sort input.txt" | getline) {
        if ($3 == "Guard") {
            id = substr($4, 2)
        } else if ($3 == "falls") {
            start = substr($2, 4, 2)
        } else if ($3 == "wakes") {
            stop = substr($2, 4, 2)
            for (i = start; i < stop; i++) {
                if (++counts[id, i] > max) {
                    max = counts[id, i]
                    ans = id * i
                }
            }
        }
    }
    print ans
}
