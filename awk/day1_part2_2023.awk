
#!/usr/bin/awk -f

BEGIN {
    total = 0
    filename = "input.txt"

    while ((getline line < filename) > 0) {
        # Use intermediate replacements to handle overlaps like "oneight" -> o1e8t
        gsub(/one/,   "o1e", line);
        gsub(/two/,   "t2o", line);
        gsub(/three/, "t3e", line);
        gsub(/four/,  "f4r", line);
        gsub(/five/,  "f5e", line);
        gsub(/six/,   "s6x", line);
        gsub(/seven/, "s7n", line);
        gsub(/eight/, "e8t", line);
        gsub(/nine/,  "n9e", line);

        # Remove all non-numeric characters
        gsub(/[^0-9]+/, "", line);

        # Extract first and last digits
        first = substr(line, 1, 1);
        last = substr(line, length(line), 1);

        # Calculate value and add to total
        total += first * 10 + last;
    }

    print total
}
