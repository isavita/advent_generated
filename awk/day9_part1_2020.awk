
BEGIN {
    preamble_size = 25
}

{
    numbers[NR] = $0

    if (NR > preamble_size) {
        valid = 0
        for (i = NR - preamble_size; i < NR; i++) {
            for (j = i + 1; j < NR; j++) {
                if (numbers[i] + numbers[j] == numbers[NR]) {
                    valid = 1
                    break
                }
            }
            if (valid) {
                break
            }
        }

        if (!valid) {
            print "First invalid number: ", numbers[NR]
            exit
        }
    }
}
