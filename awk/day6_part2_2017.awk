#!/usr/bin/awk

BEGIN {
    # Read input from input.txt
    while (getline < "input.txt" > 0) {
        # Split the input into an array
        n = split($0, banks, /\t/);
        for (i = 1; i <= n; i++) {
            banks[i] -= 0;
        }
    }

    # Initialize variables
    seen[""] = 0;
    cycles = 0;
    loop_size = 0;

    # Loop until a configuration is produced that has been seen before
    while (1) {
        # Find the bank with the most blocks
        max_blocks = 0;
        max_bank = 0;
        for (i = 1; i <= n; i++) {
            if (banks[i] > max_blocks) {
                max_blocks = banks[i];
                max_bank = i;
            }
        }

        # Redistribute the blocks
        blocks_to_redistribute = banks[max_bank];
        banks[max_bank] = 0;
        for (i = 1; i <= blocks_to_redistribute; i++) {
            max_bank = (max_bank % n) + 1;
            banks[max_bank]++;
        }

        # Convert the banks array to a string
        config = "";
        for (i = 1; i <= n; i++) {
            config = config banks[i] " ";
        }

        # Check if the configuration has been seen before
        if (config in seen) {
            loop_size = cycles - seen[config];
            break;
        }

        # Mark the configuration as seen
        seen[config] = cycles;

        # Increment the cycle count
        cycles++;
    }

    # Print the answers
    print "Part 1: " cycles;
    print "Part 2: " loop_size;
}