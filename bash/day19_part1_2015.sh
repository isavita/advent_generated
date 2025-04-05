
#!/bin/bash

# Reads replacement rules and a starting medicine molecule from input.txt.
# Calculates all distinct molecules reachable by exactly one replacement.
# Prints the count of distinct molecules.

main() {
    # Use awk for efficient text processing and associative arrays (for uniqueness).
    awk '
    # Rule lines: Store key -> list of values
    / => / {
        key = $1
        value = $3
        # Store values separated by a non-printable char (unit separator)
        # to handle potential spaces within values themselves.
        replacements[key] = replacements[key] value "\x1F"
        next # Skip to next line
    }

    # Non-empty line without " => " is the medicine string
    NF > 0 {
        medicine = $0
    }

    # After reading all input
    END {
        # Iterate through each replacement key
        for (key in replacements) {
            len_key = length(key)
            len_medicine = length(medicine)
            # Split the stored values for this key using the unit separator
            n_values = split(replacements[key], values, "\x1F")

            # Iterate through all possible start positions in the medicine string
            # awk string indices are 1-based
            for (i = 1; i <= len_medicine; i++) {
                # Check if the substring matches the current key
                if (substr(medicine, i, len_key) == key) {
                    # Construct prefix and suffix
                    prefix = substr(medicine, 1, i - 1)
                    suffix = substr(medicine, i + len_key)

                    # Apply each possible replacement value for this key
                    # Loop n_values-1 times because split creates an extra empty element at the end
                    for (j = 1; j < n_values; j++) {
                        value = values[j]
                        new_molecule = prefix value suffix
                        # Use awk array keys to store distinct molecules automatically
                        distinct_molecules[new_molecule] = 1
                    }
                }
            }
        }
        # Print the count of distinct molecules found
        print length(distinct_molecules)
    }
    ' "input.txt" # Feed the input file to awk
}

# Execute the main function
main
