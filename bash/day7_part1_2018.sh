
#!/bin/bash

# Extract rules as 2-character pairs (Prerequisite, Dependent)
constraints=$(cut -c6,37 input.txt)

# Identify all unique nodes mentioned in the rules, sorted alphabetically
nodes=$(grep -o . <<< "$constraints" | sort -u | tr -d '\n')

while [[ -n $nodes ]]; do
    for ((i=0; i<${#nodes}; i++)); do
        char=${nodes:i:1}
        
        # Check if the character is a dependency for any remaining rule
        if ! grep -q ".$char" <<< "$constraints"; then
            printf "%s" "$char"
            
            # Remove all rules where this character was the prerequisite
            constraints=$(grep -v "^$char" <<< "$constraints")
            
            # Remove this character from the list of available nodes
            nodes=${nodes/$char/}
            
            # Restart search from the beginning to maintain alphabetical priority
            continue 2
        fi
    done
done
echo
