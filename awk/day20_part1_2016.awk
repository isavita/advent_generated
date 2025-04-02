
#!/usr/bin/awk
#
# Solution for Advent of Code - Day 20: Firewall Rules (Part 1)
# Finds the lowest-valued IP address that is not blocked by any range
# specified in the input file.
#
# Input file: input.txt (each line: start-end)
# Output: Lowest allowed IP (integer) to standard output

# BEGIN block: Executed once before processing input.
# We read all input here, store it, sort it, and then process it.
# This deviates slightly from typical line-by-line AWK processing
# but is necessary because we need all ranges sorted before finding the lowest IP.
BEGIN {
    FS = "-" # Set field separator to hyphen
    count = 0  # Initialize counter for number of ranges

    # Read all lines from the input file into arrays
    # The filename "input.txt" is hardcoded as requested.
    while ( (getline < "input.txt") > 0 ) {
        # Basic validation: ensure two fields exist
        if (NF == 2) {
            count++
            # Store start and end, converting to numbers explicitly (+0)
            starts[count] = $1 + 0
            ends[count] = $2 + 0
        } else if (NF > 0) {
             print "Warning: Skipping malformed line " FNR " in input.txt: " $0 > "/dev/stderr"
        }
    }
    close("input.txt") # Close the file

    # If no valid ranges were read, the lowest allowed IP is 0
    if (count == 0) {
        print 0
        exit
    }

    # Sort the ranges based on the start IP.
    # Using a simple bubble sort; sufficient for typical puzzle inputs,
    # though inefficient (O(N^2)) for very large N.
    # A more complex sort (quicksort/mergesort) could be implemented in AWK
    # or GNU AWK's asorti could be used with index mapping, but bubble sort
    # is simple and portable standard AWK.
    for (i = 1; i <= count; i++) {
        for (j = 1; j <= count - i; j++) {
            if (starts[j] > starts[j+1]) {
                # Swap start values
                temp_s = starts[j]
                starts[j] = starts[j+1]
                starts[j+1] = temp_s

                # Swap corresponding end values
                temp_e = ends[j]
                ends[j] = ends[j+1]
                ends[j+1] = temp_e
            }
        }
    }

    # Now find the lowest allowed IP using the sorted ranges.
    # This logic implicitly merges overlapping/adjacent ranges.
    lowest_allowed = 0
    max_ip = 4294967295 # Maximum value for a 32-bit unsigned integer

    for (i = 1; i <= count; i++) {
        start = starts[i]
        end = ends[i]

        # If the current range starts *after* the IP we are checking,
        # it means 'lowest_allowed' is not blocked by this range or any previous one
        # (due to sorting and the merging logic below). We found the gap.
        if (start > lowest_allowed) {
            # Check if lowest_allowed exceeded max_ip shouldn't happen here,
            # but good practice.
             if (lowest_allowed <= max_ip) {
                print lowest_allowed
                exit # Found the lowest allowed IP
            } else {
                # This scenario implies lowest_allowed was pushed beyond max_ip
                # by previous ranges, meaning no gap exists. Fall through to END check.
                break
            }
        }

        # If the current range overlaps or continues the blocked segment including 'lowest_allowed',
        # update 'lowest_allowed' to be the next potential candidate: one greater than the end of this range.
        # We only need to update if this range extends the blocked segment further than previously seen.
        # Awk handles large integers, +1 should be safe unless end is exactly max_ip.
        next_candidate = end + 1
        if (next_candidate > lowest_allowed) {
             # Handle the case where end is max_ip; adding 1 might overflow standard floats
             # but AWK typically handles large integers well. Let's proceed.
             # If end is max_ip, next_candidate will be max_ip + 1.
             lowest_allowed = next_candidate
        }
         # If end < lowest_allowed, this range is entirely contained within a previously processed
         # merged range, so it doesn't affect the 'lowest_allowed' candidate.
    }

    # If the loop completes without finding a gap (i.e., without exiting),
    # the lowest allowed IP is the final value of 'lowest_allowed', provided it's within range.
    if (lowest_allowed <= max_ip) {
        print lowest_allowed
    } else {
        # This means all IPs from 0 to max_ip are covered by the ranges.
        print "Error: No allowed IP found within the 32-bit range." > "/dev/stderr"
        exit 1
    }
}

# No main action block (per-line processing) needed as logic is in BEGIN.
# No END block needed as logic is completed within BEGIN after reading/sorting.
