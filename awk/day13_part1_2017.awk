
#!/usr/bin/awk -f
# Awk program that reads "input.txt" and prints the severity

BEGIN {
    FS = ": "
    severity = 0
    # Open the input file
    file = "input.txt"
    while ((getline line < file) > 0) {
        split(line, a, FS)
        depth  = a[1] + 0
        range  = a[2] + 0
        period = (range - 1) * 2
        if (period > 0 && depth % period == 0)
            severity += depth * range
    }
    close(file)
}
END {
    print severity
}
