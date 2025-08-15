
#!/usr/bin/awk -f

BEGIN {
    size = 0
    while ((getline line < "input.txt") > 0) {
        offsets[size++] = line + 0
    }
    close("input.txt")

    idx = 0
    steps = 0
    while (idx >= 0 && idx < size) {
        jump = offsets[idx]
        ++offsets[idx]
        idx += jump
        ++steps
    }
    print steps
    exit
}
