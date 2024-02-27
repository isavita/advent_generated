
BEGIN {
    count = 0
    while ((getline line < "input.txt") > 0) {
        split(line, ranges, ",")
        if (length(ranges) != 2) {
            continue
        }
        split(ranges[1], parts1, "-")
        split(ranges[2], parts2, "-")
        start1 = parts1[1]
        end1 = parts1[2]
        start2 = parts2[1]
        end2 = parts2[2]
        if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)) {
            count++
        }
    }
    print count
}
