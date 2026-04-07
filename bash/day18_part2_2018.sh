
#!/bin/bash

awk '
{
    grid[NR] = $0
    N = NR
    M = length($0)
}
END {
    target = 1000000000
    for (m = 0; m < target; m++) {
        state = ""
        for (i = 1; i <= N; i++) state = state grid[i]
        
        if (state in seen) {
            cycle_len = m - seen[state]
            m += int((target - m) / cycle_len) * cycle_len
            if (m >= target) break
        }
        seen[state] = m

        for (i = 1; i <= N; i++) {
            new_row = ""
            for (j = 1; j <= M; j++) {
                trees = 0
                lumber = 0
                curr = substr(grid[i], j, 1)
                
                for (di = -1; di <= 1; di++) {
                    ni = i + di
                    if (ni < 1 || ni > N) continue
                    for (dj = -1; dj <= 1; dj++) {
                        nj = j + dj
                        if ((di == 0 && dj == 0) || nj < 1 || nj > M) continue
                        char = substr(grid[ni], nj, 1)
                        if (char == "|") trees++
                        else if (char == "#") lumber++
                    }
                }

                if (curr == "." && trees >= 3) next_char = "|"
                else if (curr == "|" && lumber >= 3) next_char = "#"
                else if (curr == "#") {
                    if (lumber >= 1 && trees >= 1) next_char = "#"
                    else next_char = "."
                } else next_char = curr
                
                new_row = new_row next_char
            }
            next_grid[i] = new_row
        }
        for (i = 1; i <= N; i++) grid[i] = next_grid[i]
    }

    final_trees = 0
    final_lumber = 0
    for (i = 1; i <= N; i++) {
        row = grid[i]
        final_trees += gsub(/\|/, "|", row)
        row = grid[i]
        final_lumber += gsub(/#/, "#", row)
    }
    print final_trees * final_lumber
}' input.txt
