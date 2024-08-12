BEGIN {
    # Read the polymer template
    getline polymer < "input.txt"
    
    # Read the rules into an associative array
    while (getline < "input.txt") {
        if ($0 ~ /->/) {
            rule[substr($1, 1, 2)] = $3
        }
    }
    
    # Perform 10 steps of pair insertion
    for (step = 1; step <= 10; step++) {
        new_polymer = ""
        for (i = 1; i < length(polymer); i++) {
            pair = substr(polymer, i, 2)
            new_polymer = new_polymer substr(polymer, i, 1)
            if (pair in rule) {
                new_polymer = new_polymer rule[pair]
            }
        }
        new_polymer = new_polymer substr(polymer, length(polymer), 1)
        polymer = new_polymer
    }
    
    # Count occurrences of each element
    for (i = 1; i <= length(polymer); i++) {
        count[substr(polymer, i, 1)]++
    }
    
    # Find the most and least common elements
    max_count = 0
    min_count = length(polymer)
    for (element in count) {
        if (count[element] > max_count) max_count = count[element]
        if (count[element] < min_count) min_count = count[element]
    }
    
    # Print the difference
    print max_count - min_count
}