
awk '
BEGIN {
    getline line < "input.txt"
    n = split(line, banks)
    
    while (1) {
        state = ""
        for (i = 1; i <= n; i++) state = state banks[i] "_"
        if (seen[state]) break
        seen[state] = 1

        max_idx = 1
        for (i = 2; i <= n; i++) {
            if (banks[i] > banks[max_idx]) max_idx = i
        }

        blocks = banks[max_idx]
        banks[max_idx] = 0
        curr = max_idx
        while (blocks > 0) {
            curr = (curr % n) + 1
            banks[curr]++
            blocks--
        }
        cycles++
    }
    print cycles
}'
