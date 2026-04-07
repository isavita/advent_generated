
awk '
{
    for (i=1; i<=length($0); i++) grid[NR, i] = substr($0, i, 1)
    W = length($0); H = NR
}
END {
    for (m=1; m<=10; m++) {
        for (r=1; r<=H; r++) {
            for (c=1; c<=W; c++) {
                t=0; l=0
                for (dr=-1; dr<=1; dr++) {
                    for (dc=-1; dc<=1; dc++) {
                        if (dr==0 && dc==0) continue
                        char = grid[r+dr, c+dc]
                        if (char == "|") t++
                        else if (char == "#") l++
                    }
                }
                curr = grid[r, c]
                if (curr == "." && t >= 3) next_grid[r, c] = "|"
                else if (curr == "|" && l >= 3) next_grid[r, c] = "#"
                else if (curr == "#") next_grid[r, c] = (l >= 1 && t >= 1) ? "#" : "."
                else next_grid[r, c] = curr
            }
        }
        for (k in next_grid) grid[k] = next_grid[k]
    }
    for (r=1; r<=H; r++) {
        for (c=1; c<=W; c++) {
            if (grid[r, c] == "|") tw++
            if (grid[r, c] == "#") tl++
        }
    }
    print tw * tl
}' input.txt
