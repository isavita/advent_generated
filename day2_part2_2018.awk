
{
    lines[NR] = $0
}
END {
    for (i = 1; i < NR; i++) {
        for (j = i + 1; j <= NR; j++) {
            diff = 0
            for (k = 1; k <= length(lines[i]); k++) {
                if (substr(lines[i], k, 1) != substr(lines[j], k, 1)) {
                    diff++
                    if (diff > 1) {
                        break
                    }
                }
            }
            if (diff == 1) {
                common = ""
                for (k = 1; k <= length(lines[i]); k++) {
                    if (substr(lines[i], k, 1) == substr(lines[j], k, 1)) {
                        common = common substr(lines[i], k, 1)
                    }
                }
                print common
                exit
            }
        }
    }
}
