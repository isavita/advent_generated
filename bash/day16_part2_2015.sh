
#!/bin/bash

main() {
    awk '
    BEGIN {
        # Define target values
        mfcsam["children"] = 3; mfcsam["cats"] = 7; mfcsam["samoyeds"] = 2;
        mfcsam["pomeranians"] = 3; mfcsam["akitas"] = 0; mfcsam["vizslas"] = 0;
        mfcsam["goldfish"] = 5; mfcsam["trees"] = 3; mfcsam["cars"] = 2;
        mfcsam["perfumes"] = 1;
        FS = "[ :,]+"; # Set Field Separator to space, colon, or comma
    }

    {
        sue_num = $2;
        valid = 1; # Assume valid for this line

        # Loop through property-value pairs (fields 3, 5, 7, ...)
        for (i = 3; i < NF; i += 2) {
            prop = $i;
            val = $(i+1);
            target_val = mfcsam[prop];

            # Apply comparison rules - use "next" on failure to skip to next line
            if (prop == "cats" || prop == "trees") {
                if (val <= target_val) { valid = 0; next; }
            } else if (prop == "pomeranians" || prop == "goldfish") {
                if (val >= target_val) { valid = 0; next; }
            } else {
                if (val != target_val) { valid = 0; next; }
            }
        }

        # If loop completed without "next", this Sue is the match
        if (valid == 1) {
             print sue_num;
             exit; # Found the match, exit awk
        }
    }
    ' "input.txt"
}

main
