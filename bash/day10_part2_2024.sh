
#!/bin/bash

main() {
    awk '
        # DFS function with memoization
        # Parameters: r (row), c (column)
        # Local variables declared after params: h, sum_paths, i, dr, dc, nr2, nc2
        function dfs(r, c,    h, sum_paths, i, dr1, dr2, dr3, dr4, dc1, dc2, dc3, dc4, nr2, nc2) {
            # Memoization check: if result for (r,c) is already computed, return it
            if ((r SUBSEP c) in dp) {
                return dp[r,c]
            }

            h = grid[r,c] # Current height

            # Base case: if height is 9, this path ends here, counts as 1 path
            if (h == 9) {
                dp[r,c] = 1
                return 1
            }

            # Directions: (1,0), (-1,0), (0,1), (0,-1) stored directly
            dr1=1; dc1=0
            dr2=-1; dc2=0
            dr3=0; dc3=1
            dr4=0; dc4=-1

            sum_paths = 0

            # Check neighbor 1 (down)
            nr2 = r + dr1; nc2 = c + dc1
            if (nr2 >= 1 && nr2 <= max_r && nc2 >= 1 && nc2 <= max_c && (nr2 SUBSEP nc2) in grid) {
                if (grid[nr2, nc2] == h + 1) {
                    sum_paths += dfs(nr2, nc2)
                }
            }
            # Check neighbor 2 (up)
            nr2 = r + dr2; nc2 = c + dc2
             if (nr2 >= 1 && nr2 <= max_r && nc2 >= 1 && nc2 <= max_c && (nr2 SUBSEP nc2) in grid) {
                if (grid[nr2, nc2] == h + 1) {
                    sum_paths += dfs(nr2, nc2)
                }
            }
             # Check neighbor 3 (right)
            nr2 = r + dr3; nc2 = c + dc3
             if (nr2 >= 1 && nr2 <= max_r && nc2 >= 1 && nc2 <= max_c && (nr2 SUBSEP nc2) in grid) {
                if (grid[nr2, nc2] == h + 1) {
                    sum_paths += dfs(nr2, nc2)
                }
            }
             # Check neighbor 4 (left)
            nr2 = r + dr4; nc2 = c + dc4
             if (nr2 >= 1 && nr2 <= max_r && nc2 >= 1 && nc2 <= max_c && (nr2 SUBSEP nc2) in grid) {
                if (grid[nr2, nc2] == h + 1) {
                    sum_paths += dfs(nr2, nc2)
                }
            }

            # Store result in memoization table (dp) and return it
            dp[r,c] = sum_paths
            return sum_paths
        }

        # Action for each line read from input.txt
        {
            # Store max row number encountered so far
            max_r = NR
            # If first line, store max column number
            if (NR == 1) {
                 # FS="" splits into characters; NF is number of chars/cols
                 max_c = NF
            }
            # Populate the grid associative array: grid[row, col] = value
            for (c = 1; c <= NF; ++c) {
                grid[NR, c] = $c
            }
        }

        # END block: executed after all lines are processed
        END {
            total = 0
            # Iterate through the entire grid
            for (r = 1; r <= max_r; ++r) {
                for (c = 1; c <= max_c; ++c) {
                    # If a cell has value 0, start DFS from here and add result to total
                    if ((r SUBSEP c) in grid && grid[r,c] == 0) {
                        total += dfs(r, c)
                    }
                }
            }
            # Print the final total number of paths
            print total
        }
    # Set Field Separator to empty string to split line into individual characters
    # Read input from "input.txt"
    ' FS="" < "input.txt"
}

# Execute the main function
main
