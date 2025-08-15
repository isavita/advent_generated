#!/usr/bin/awk -f
BEGIN{
    while ((getline line < "input.txt") > 0) {
        changes[++n] = line + 0
    }
    close("input.txt")
    current = 0
    seen[current] = 1
    while (1) {
        for (i = 1; i <= n; i++) {
            current += changes[i]
            if (current in seen) {
                print current
                exit
            }
            seen[current] = 1
        }
    }
}