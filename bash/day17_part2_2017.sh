
#!/usr/bin/env bash

# Read the step value from input.txt
steps=$(<input.txt)

# Compute the value that ends up after 0 in the circular buffer
awk -v steps="$steps" '
BEGIN {
    pos = 0
    ans = 0
    for (i = 1; i <= 50000000; i++) {
        pos = (pos + steps) % i
        if (pos == 0) ans = i
        pos++
    }
    print ans
}
'
