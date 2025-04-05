
#!/bin/bash

# Reads input from input.txt and solves the problem

main() {
    # Use awk for efficient array handling and arithmetic
    awk '
    NR==1 {
        # Calculate the target number of presents (input / 11)
        target = int($0 / 11)

        # Simulate elves delivering presents
        # Outer loop: Iterate through elves
        for (elf = 1; elf <= target; elf++) {
            # Calculate the limit for houses this elf visits (min(elf*50, target))
            limit = elf * 50
            if (limit > target) {
                limit = target
            }

            # Inner loop: Iterate through houses the elf visits
            for (house = elf; house <= limit; house += elf) {
                # Add presents to the house total (awk arrays default to 0)
                houses[house] += elf
            }
        }

        # Find the first house with enough presents
        for (house_number = 1; house_number <= target; house_number++) {
            # Check if the house received at least the target number of presents
            if (houses[house_number] >= target) {
                # Print the house number and exit
                print house_number
                exit
            }
        }
    }
    ' input.txt
}

# Call the main function
main
