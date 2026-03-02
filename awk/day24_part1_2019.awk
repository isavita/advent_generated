
BEGIN {
    ARGC = 2
    ARGV[1] = "input.txt"
    FS = ""
}
{
    for (i = 1; i <= NF; i++) g[++n] = $i
}
END {
    while (1) {
        s = ""
        for (i = 1; i <= 25; i++) s = s g[i]
        if (s in seen) {
            score = 0; p = 1
            for (i = 1; i <= 25; i++) {
                if (substr(s, i, 1) == "#") score += p
                p *= 2
            }
            printf "%.0f\n", score
            exit
        }
        seen[s] = 1
        for (i = 1; i <= 25; i++) {
            c = (i > 5 && g[i-5] == "#") + (i <= 20 && g[i+5] == "#") + \
                ((i-1) % 5 != 0 && g[i-1] == "#") + (i % 5 != 0 && g[i+1] == "#")
            if (g[i] == "#") nx[i] = (c == 1 ? "#" : ".")
            else nx[i] = (c == 1 || c == 2 ? "#" : ".")
        }
        for (i = 1; i <= 25; i++) g[i] = nx[i]
    }
}
