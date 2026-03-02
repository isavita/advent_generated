
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    W = 101; H = 103; MX = 50; MY = 51
}
{
    gsub(/[^0-9-]/, " ")
    px[NR] = $1; py[NR] = $2; vx[NR] = $3; vy[NR] = $4
}
END {
    for (i = 1; i <= NR; i++) {
        x = (px[i] + 100 * vx[i]) % W; if (x < 0) x += W
        y = (py[i] + 100 * vy[i]) % H; if (y < 0) y += H
        if (x < MX) {
            if (y < MY) q1++
            else if (y > MY) q3++
        } else if (x > MX) {
            if (y < MY) q2++
            else if (y > MY) q4++
        }
    }
    printf "Part 1 - Safety Factor after 100 seconds: %d\n", q1 * q2 * q3 * q4

    for (t = 1; t <= W * H; t++) {
        delete s; o = 0
        for (i = 1; i <= NR; i++) {
            x = (px[i] + t * vx[i]) % W; if (x < 0) x += W
            y = (py[i] + t * vy[i]) % H; if (y < 0) y += H
            if (s[x, y]++) { o = 1; break }
        }
        if (!o) {
            printf "Part 2 - Fewest seconds to display Easter egg: %d\n", t
            printf "Final positions of robots:\n"
            for (y = 0; y < H; y++) {
                for (x = 0; x < W; x++) printf (s[x, y] ? "#" : ".")
                printf "\n"
            }
            exit
        }
    }
}
