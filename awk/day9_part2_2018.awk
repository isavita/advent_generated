
#!/usr/bin/awk -f
# Marble game – reads the first line of input.txt,
# multiplies the last‑marble value by 100,
# simulates the game and prints the highest score.

BEGIN {
    # read the single line from input.txt
    while ((getline line < "input.txt") > 0) {
        split(line, p, " ")
        players = p[1] + 0               # number of players
        lastMarble = p[7] + 0            # the 7th token is the last marble value
        close("input.txt")
        break
    }

    maxMarble = lastMarble * 100

    # initialise circular list with marble 0
    left[0] = right[0] = 0
    cur = 0

    # scores[0..players-1] = 0
    for (i = 0; i < players; i++) scores[i] = 0

    # main loop
    for (marble = 1; marble <= maxMarble; marble++) {
        if (marble % 23 == 0) {
            player = (marble - 1) % players
            # move 7 steps counter‑clockwise
            for (i = 0; i < 7; i++) cur = left[cur]

            scores[player] += marble + cur

            # remove cur from the circle
            l = left[cur]; r = right[cur]
            right[l] = r
            left[r]  = l
            cur = r                     # new current marble
        } else {
            # move one step clockwise
            cur = right[cur]

            # insert new marble between cur and its clockwise neighbour
            new = marble
            l = cur; r = right[cur]
            left[new]  = l
            right[new] = r
            right[l]   = new
            left[r]    = new
            cur = new
        }
    }

    # find maximum score
    maxScore = 0
    for (i = 0; i < players; i++) {
        if (scores[i] > maxScore) maxScore = scores[i]
    }

    print maxScore
}
