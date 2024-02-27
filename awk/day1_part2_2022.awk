
BEGIN {
    FS = "\n"
    RS = ""
}

{
    cal = 0
    for (i = 1; i <= NF; i++) {
        if ($i != "") {
            cal += $i
        }
    }
    if (cal > max1) {
        max3 = max2
        max2 = max1
        max1 = cal
    } else if (cal > max2) {
        max3 = max2
        max2 = cal
    } else if (cal > max3) {
        max3 = cal
    }
}

END {
    print "Part 1: " max1
    print "Part 2: " max1 + max2 + max3
}
