
#!/usr/bin/awk -f

function solve(target, current, idx) {
    if (idx > NF) return (current == target)
    if (current > target) return 0
    return solve(target, current + $idx, idx + 1) || solve(target, current * $idx, idx + 1)
}

BEGIN {
    FS = "[: ]+"
    ARGV[1] = "input.txt"
    ARGC = 2
}

{
    if (solve($1, $2, 3)) {
        total += $1
    }
}

END {
    printf "%.0f\n", total
}
