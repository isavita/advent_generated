
{
    numbers[NR] = $1
}

END {
    invalidNumber = 14360655

    for (i = 1; i <= NR; i++) {
        sum = numbers[i]
        min = numbers[i]
        max = numbers[i]
        for (j = i + 1; j <= NR; j++) {
            sum += numbers[j]
            if (numbers[j] < min) {
                min = numbers[j]
            }
            if (numbers[j] > max) {
                max = numbers[j]
            }
            if (sum == invalidNumber) {
                print min + max
                exit
            } else if (sum > invalidNumber) {
                break
            }
        }
    }
}
