
#!/bin/bash

main() {
    input=$(<input.txt)
    offset=$((10#${input:0:7})) # Ensure base 10 interpretation

    awk -v input="$input" -v offset="$offset" '
    BEGIN {
        n = length(input)
        total_len = n * 10000
        suffix_len = total_len - offset

        # Build only the relevant suffix of the full repeated input
        # digits[j] corresponds to the element at original index (offset + j)
        for (j = 0; j < suffix_len; ++j) {
            original_index = (offset + j) % n
            digits[j] = substr(input, original_index + 1, 1) # AWK substr is 1-based
        }

        # Run 100 phases on the relevant suffix
        for (phase = 0; phase < 100; ++phase) {
            total = 0
            # Iterate backwards through the suffix
            for (i = suffix_len - 1; i >= 0; --i) {
                total = (total + digits[i]) % 10
                digits[i] = total
            }
        }

        # Extract the first 8 digits of the result (which start at the offset)
        result = ""
        for (i = 0; i < 8; ++i) {
            result = result digits[i]
        }
        print result
    }
    '
}

main
