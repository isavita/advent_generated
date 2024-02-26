
# Read input from file
BEGIN {
    FS="\n"
    while ((getline line < "input.txt") > 0) {
        measurements[++i] = line
    }
}

# Part 1: Count depth increases
{
    increases = 0
    for (j = 2; j <= i; j++) {
        if (measurements[j] > measurements[j - 1]) {
            increases++
        }
    }
    print "Part 1: Number of depth increases: " increases
}

# Part 2: Count sliding window sum increases
{
    sum_increases = 0
    for (k = 1; k <= i - 2; k++) {
        window_sum[k] = measurements[k] + measurements[k + 1] + measurements[k + 2]
        if (k > 1 && window_sum[k] > window_sum[k - 1]) {
            sum_increases++
        }
    }
    print "Part 2: Number of sliding window sum increases: " sum_increases
}
