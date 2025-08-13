
#!/usr/bin/awk -f
#  awk solution for the cookie‑score problem
#  Reads data from input.txt and prints the maximum score
#  with 100 teaspoons and exactly 500 calories.

BEGIN {
    # Read ingredients from input.txt
    n = 0
    while ((getline line < "input.txt") > 0) {
        split(line, a, ": ")
        rest = a[2]
        gsub(/, /, " ", rest)          # replace ", " with space
        split(rest, b, " ")
        capacity[n]   = b[2]
        durability[n] = b[4]
        flavor[n]    = b[6]
        texture[n]   = b[8]
        calories[n]  = b[10]
        n++
    }

    maxScore = 0
    rec(0, 100)          # start recursion
    print maxScore
}

# Recursive search over all distributions of teaspoons
function rec(idx, rem,   i) {
    if (idx == n-1) {
        teaspoons[idx] = rem
        if (calcCalories() == 500) {
            t = calcScore()
            if (t > maxScore) maxScore = t
        }
        return
    }
    for (i = 0; i <= rem; i++) {
        teaspoons[idx] = i
        rec(idx+1, rem-i)
    }
}

# Compute total calories for current distribution
function calcCalories() {
    cal = 0
    for (i = 0; i < n; i++) cal += calories[i] * teaspoons[i]
    return cal
}

# Compute score for current distribution
function calcScore() {
    cap = 0; dur = 0; fla = 0; tex = 0
    for (i = 0; i < n; i++) {
        cap += capacity[i]   * teaspoons[i]
        dur += durability[i] * teaspoons[i]
        fla += flavor[i]    * teaspoons[i]
        tex += texture[i]   * teaspoons[i]
    }
    if (cap < 0) cap = 0
    if (dur < 0) dur = 0
    if (fla < 0) fla = 0
    if (tex < 0) tex = 0
    return cap * dur * fla * tex
}
