
#!/bin/bash

# This script calculates the minimum steps for the "Elevator and Items" puzzle (Advent of Code 2016 Day 11).
# It uses the mathematical optimization for the "Elevator" problem: moving N items up one floor takes 2N - 3 steps.

awk '{
    # Count occurrences of "generator" and "microchip" in each line
    count = gsub(/generator|microchip/, "X")
    
    # Cumulative sum of items found on current and previous floors
    items_count += count
    
    # Apply the 2N - 3 rule for each transition between floors (1 to 2, 2 to 3, 3 to 4)
    # The first 3 floors contribute to the steps required to move items to the 4th.
    if (NR < 4 && items_count > 0) {
        steps += (2 * items_count - 3)
    }
} END {
    print steps
}' input.txt
