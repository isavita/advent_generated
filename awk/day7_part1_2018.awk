
#!/usr/bin/awk -f

# Topological sort of steps A‑Z from "input.txt"
# Prints the ordering as a single string.

BEGIN {
    # initialise indegrees for all 26 steps
    for (i = 0; i < 26; i++) indegree[i] = 0

    # read the constraints
    while ((getline line < "input.txt") > 0) {
        # lines are: "Step X must be finished before step Y can begin."
        step1 = substr(line, 6, 1)   # first step letter
        step2 = substr(line, 37, 1)  # second step letter

        i = index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", step1) - 1
        j = index("ABCDEFGHIJKLMNOPQRSTUVWXYZ", step2) - 1

        graph[i, j] = 1               # edge i → j
        indegree[j]++                  # count incoming edges for j
    }

    # topological sort (lexicographically smallest available step)
    completed = 0
    order = ""

    while (completed < 26) {
        for (i = 0; i < 26; i++) {
            if (indegree[i] == 0) {
                # output this step
                order = order sprintf("%c", i + 65)
                indegree[i] = -1        # mark as processed
                completed++

                # remove its outgoing edges
                for (j = 0; j < 26; j++) {
                    if ((i, j) in graph) {
                        indegree[j]--
                    }
                }
                break                     # restart scan from A
            }
        }
    }

    print order
}
