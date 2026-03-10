
#!/usr/bin/awk -f

# Setup the target values from the MFCSAM output
BEGIN {
    target["children"] = 3
    target["cats"] = 7
    target["samoyeds"] = 2
    target["pomeranians"] = 3
    target["akitas"] = 0
    target["vizslas"] = 0
    target["goldfish"] = 5
    target["trees"] = 3
    target["cars"] = 2
    target["perfumes"] = 1

    # Ensure the script reads from input.txt if no file is provided
    if (ARGC == 1) {
        ARGV[1] = "input.txt"
        ARGC = 2
    }
}

{
    # Each line looks like: Sue 1: goldfish: 6, trees: 9, akitas: 0
    # Strip punctuation to make field processing easier
    gsub(/[:,]/, "", $0)
    
    # $2 is the Sue ID
    sue_id = $2
    
    is_part1 = 1
    is_part2 = 1
    
    # Loop through attribute-value pairs (starting at field 3)
    for (i = 3; i < NF; i += 2) {
        key = $i
        val = $(i+1)
        
        # Part 1 Logic: Values must match exactly
        if (target[key] != val) {
            is_part1 = 0
        }
        
        # Part 2 Logic: 
        # cats and trees are 'greater than'
        # pomeranians and goldfish are 'fewer than'
        # others are exact matches
        if (key == "cats" || key == "trees") {
            if (val <= target[key]) is_part2 = 0
        } else if (key == "pomeranians" || key == "goldfish") {
            if (val >= target[key]) is_part2 = 0
        } else {
            if (val != target[key]) is_part2 = 0
        }
    }
    
    if (is_part1) part1_result = sue_id
    if (is_part2) part2_result = sue_id
}

END {
    printf "Part 1 (The Sue that matches exactly): %s\n", part1_result
    printf "Part 2 (The Sue based on range adjustments): %s\n", part2_result
}
