
BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($0 ~ /\[/) {
            n = int((length($0) + 1) / 4)
            if (n > max) max = n
            for (i = 1; i <= n; i++) {
                char = substr($0, 4 * i - 2, 1)
                if (char != " ") s[i] = char s[i]
            }
        } else if ($1 == "move") {
            for (i = 1; i <= $2; i++) {
                len = length(s[$4])
                s[$6] = s[$6] substr(s[$4], len, 1)
                s[$4] = substr(s[$4], 1, len - 1)
            }
        }
    }
    for (i = 1; i <= max; i++) {
        printf "%s", substr(s[i], length(s[i]), 1)
    }
    printf "\n"
}
