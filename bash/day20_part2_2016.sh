
#!/bin/bash

# Solution Part 2: Count allowed IPs

main() {
    # Sort ranges numerically by start IP, then process with awk
    sort -t'-' -n -k1,1 input.txt | awk -F'-' '
    BEGIN {
        current_max = 0
        allowed_ips = 0
    }
    {
        start = $1
        end = $2
        # If there is a gap between the last max and the current start
        if (start > current_max + 1) {
            allowed_ips += start - current_max - 1
        }
        # Update the maximum blocked IP if the current range extends further
        if (end > current_max) {
            current_max = end
        }
    }
    END {
        # Add IPs allowed after the last blocked range up to the maximum 32-bit integer
        allowed_ips += 4294967295 - current_max
        print allowed_ips
    }'
}

main
