
#!/usr/bin/awk -f

# Read the whole input line from input.txt
BEGIN {
    # Read the first (and only) line
    getline line < "input.txt"

    # ---------- Pass 1: total size ----------
    total = 0
    for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)
        if (c ~ /[0-9]/) total += c + 0
    }

    if (total == 0) { print 0; exit }

    # ---------- Pass 2: build disk ----------
    pos = 0
    digit_idx = 0
    for (i = 1; i <= length(line); i++) {
        c = substr(line, i, 1)
        if (c ~ /[0-9]/) {
            len = c + 0
            if (digit_idx % 2 == 0) {               # file segment
                file_id = int(digit_idx / 2)
                for (j = 0; j < len; j++) disk[pos++] = file_id
            } else {                                 # free space
                for (j = 0; j < len; j++) {
                    disk[pos++] = -1
                }
            }
            digit_idx++
        }
    }

    # ---------- Compaction ----------
    left = 0
    right = total - 1
    while (left < right) {
        while (left < right && disk[left] != -1) left++
        while (left < right && disk[right] == -1) right--
        if (left < right) {
            tmp = disk[left]
            disk[left] = disk[right]
            disk[right] = tmp
            left++
            right--
        }
    }

    # ---------- Checksum ----------
    checksum = 0
    for (i = 0; i < total; i++) {
        if (disk[i] != -1) checksum += i * disk[i]
    }

    print checksum
    exit
}
