
#!/usr/bin/awk -f

BEGIN {
    # Read lengths from input.txt
    i = 0
    while ((getline line < "input.txt") > 0) {
        n = split(line, arr, ",")
        for (j = 1; j <= n; j++) {
            if (arr[j] != "")
                lengths[i++] = arr[j] + 0
        }
    }
    close("input.txt")

    # Initialize list
    for (j = 0; j < 256; j++) list[j] = j
    current = 0
    skip = 0

    # Perform the knotting algorithm
    for (j = 0; j < i; j++) {
        len = lengths[j]
        half = int(len / 2)
        for (k = 0; k < half; k++) {
            start = (current + k) % 256
            end   = (current + len - 1 - k) % 256
            tmp = list[start]
            list[start] = list[end]
            list[end] = tmp
        }
        current = (current + len + skip) % 256
        skip++
    }

    # Output result
    print list[0] * list[1]
}
