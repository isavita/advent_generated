
#!/bin/bash

# Main function encapsulates the logic
main() {
    # Read only the molecule string from the input file "input.txt"
    # Awk: Skip lines until an empty line is found (f=1), then print the next line (the molecule) and exit.
    local molecule
    molecule=$(awk '/^$/{f=1; next} f{print; exit}' input.txt)

    # Count total elements (atoms) using grep
    # -o: print only matching parts
    # '[A-Z][a-z]*': match uppercase letter possibly followed by lowercase letters
    # wc -l: count the lines (each line is one matched element)
    local total_atoms
    total_atoms=$(grep -o '[A-Z][a-z]*' <<< "$molecule" | wc -l)

    # Count occurrences of Rn using grep -o and wc -l
    local count_rn
    count_rn=$(grep -o 'Rn' <<< "$molecule" | wc -l)

    # Count occurrences of Ar using grep -o and wc -l
    local count_ar
    count_ar=$(grep -o 'Ar' <<< "$molecule" | wc -l)

    # Count occurrences of Y using grep -o and wc -l
    local count_y
    count_y=$(grep -o 'Y' <<< "$molecule" | wc -l)

    # Apply the formula derived for this specific problem structure:
    # Steps = TotalAtoms - Count(Rn) - Count(Ar) - 2 * Count(Y) - 1
    local steps
    steps=$((total_atoms - count_rn - count_ar - 2 * count_y - 1))

    echo "$steps"
}

# Execute the main function
main
