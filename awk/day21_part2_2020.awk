
#!/usr/bin/awk -f

# Day 21: Allergen Assessment
# This program determines which ingredients cannot contain allergens 
# and identifies the specific ingredient for each listed allergen.

BEGIN {
    # Ensure the program reads from input.txt by default
    if (ARGC < 2) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }
}

{
    # Each line format: [ingredients...] (contains [allergen1], [allergen2]...)
    line = $0
    sub(/\)/, "", line)
    split_idx = index(line, " (contains ")
    
    # Separate the ingredients from the allergen list
    if (split_idx == 0) {
        ing_str = line
        n_alg = 0
    } else {
        ing_str = substr(line, 1, split_idx - 1)
        # "(contains " is 11 characters long
        alg_str = substr(line, split_idx + 11)
        gsub(/,/, "", alg_str)
        n_alg = split(alg_str, algs, " ")
    }
    
    n_ing = split(ing_str, ings, " ")

    # Maintain counts and existence tracking
    delete curr_food_ings
    for (i = 1; i <= n_ing; i++) {
        ing = ings[i]
        total_counts[ing]++
        all_unique_ings[ing] = 1
        curr_food_ings[ing] = 1
    }

    # If an allergen is listed, the ingredient containing it must be in the current food's list.
    # We maintain a potential set for each allergen via intersection.
    for (i = 1; i <= n_alg; i++) {
        a = algs[i]
        all_unique_algs[a] = 1
        if (!(a in seen_algs)) {
            # First time seeing this allergen: all current ingredients are potential candidates
            seen_algs[a] = 1
            for (ing in curr_food_ings) {
                potential[a, ing] = 1
            }
        } else {
            # Intersection: ingredient must be present in every food associated with this allergen
            for (ing in all_unique_ings) {
                if (((a, ing) in potential) && !(ing in curr_food_ings)) {
                    delete potential[a, ing]
                }
            }
        }
    }
}

END {
    # Part 1: Count occurrences of ingredients that cannot possibly contain any allergen.
    # An ingredient is "possible" if it remains in the potential set of at least one allergen.
    sum_safe = 0
    for (ing in all_unique_ings) {
        possible = 0
        for (a in all_unique_algs) {
            if ((a, ing) in potential) {
                possible = 1
                break
            }
        }
        if (!possible) {
            sum_safe += total_counts[ing]
        }
    }
    print sum_safe

    # Part 2: Deduce which allergen belongs to which ingredient (Sudoku-style elimination)
    total_algs_count = 0
    for (a in all_unique_algs) total_algs_count++
    
    resolved_count = 0
    while (resolved_count < total_algs_count) {
        for (a in all_unique_algs) {
            if (a in final_mapping) continue
            
            # Count remaining possible ingredients for this allergen
            options_count = 0
            candidate = ""
            for (ing in all_unique_ings) {
                if ((a, ing) in potential) {
                    options_count++
                    candidate = ing
                }
            }
            
            # If only one candidate remains, it is confirmed as the allergen's source
            if (options_count == 1) {
                final_mapping[a] = candidate
                resolved_count++
                # This ingredient can no longer be a candidate for any other allergen
                for (other_a in all_unique_algs) {
                    if (other_a != a) {
                        delete potential[other_a, candidate]
                    }
                }
            }
        }
    }

    # Sort the resolved allergens alphabetically
    n = 0
    for (a in final_mapping) sorted_algs[++n] = a
    for (i = 1; i <= n; i++) {
        for (j = i + 1; j <= n; j++) {
            if (sorted_algs[i] > sorted_algs[j]) {
                tmp = sorted_algs[i]
                sorted_algs[i] = sorted_algs[j]
                sorted_algs[j] = tmp
            }
        }
    }

    # Generate the canonical dangerous ingredient list
    for (i = 1; i <= n; i++) {
        printf "%s%s", final_mapping[sorted_algs[i]], (i == n ? "" : ",")
    }
    printf "\n"
}
