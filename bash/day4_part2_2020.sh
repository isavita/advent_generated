
#!/bin/bash

main() {
    awk '
    BEGIN {
        RS = "\n\n"  # Records separated by blank lines
        FS = "[ \n]+" # Fields separated by space or newline within a record
        valid_count = 0
    }

    {
        delete fields # Clear fields from previous passport
        for (i = 1; i <= NF; i++) {
            if ($i != "") { # Handle potential empty fields from splitting
                split($i, kv, ":")
                if (kv[1] != "") { # Ensure key is not empty
                    fields[kv[1]] = kv[2]
                }
            }
        }

        # Presence check
        if (!("byr" in fields && "iyr" in fields && "eyr" in fields && \
              "hgt" in fields && "hcl" in fields && "ecl" in fields && \
              "pid" in fields)) {
            next # Skip to next passport if required fields missing
        }

        # Value validation
        byr = fields["byr"]; iyr = fields["iyr"]; eyr = fields["eyr"]
        hgt = fields["hgt"]; hcl = fields["hcl"]; ecl = fields["ecl"]; pid = fields["pid"]

        valid = 1 # Assume valid until a check fails

        # Validate byr, iyr, eyr
        if (!(byr ~ /^[0-9]{4}$/ && byr >= 1920 && byr <= 2002)) valid = 0
        if (valid && !(iyr ~ /^[0-9]{4}$/ && iyr >= 2010 && iyr <= 2020)) valid = 0
        if (valid && !(eyr ~ /^[0-9]{4}$/ && eyr >= 2020 && eyr <= 2030)) valid = 0

        # Validate hgt
        if (valid) {
            if (hgt ~ /^[0-9]+cm$/) {
                h = substr(hgt, 1, length(hgt)-2) + 0 # Force numeric
                if (!(h >= 150 && h <= 193)) valid = 0
            } else if (hgt ~ /^[0-9]+in$/) {
                h = substr(hgt, 1, length(hgt)-2) + 0 # Force numeric
                if (!(h >= 59 && h <= 76)) valid = 0
            } else {
                valid = 0 # Invalid format
            }
        }

        # Validate hcl, ecl, pid
        if (valid && !(hcl ~ /^#[0-9a-f]{6}$/)) valid = 0
        if (valid && !(ecl ~ /^(amb|blu|brn|gry|grn|hzl|oth)$/)) valid = 0
        if (valid && !(pid ~ /^[0-9]{9}$/)) valid = 0

        # Increment count if all checks passed
        if (valid) {
            valid_count++
        }
    }

    END {
        print valid_count
    }
    ' "input.txt"
}

main
