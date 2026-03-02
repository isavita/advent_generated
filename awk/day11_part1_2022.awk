
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
    m = -1
}
/^Monkey/ {
    m++
    monkeys[m, "n"] = 0
    monkeys[m, "s"] = 0
    monkeys[m, "ins"] = 0
    next
}
/Starting items:/ {
    sub(/.*: /, "")
    gsub(/,/, "")
    cnt = split($0, items)
    for (i = 1; i <= cnt; i++) {
        monkeys[m, monkeys[m, "n"]++] = items[i]
    }
    next
}
/Operation:/ {
    monkeys[m, "op"] = $(NF-1)
    monkeys[m, "arg"] = $NF
    next
}
/Test:/ { monkeys[m, "div"] = $NF; next }
/If true:/ { monkeys[m, "t"] = $NF; next }
/If false:/ { monkeys[m, "f"] = $NF; next }
END {
    num_m = m + 1
    for (r = 1; r <= 20; r++) {
        for (i = 0; i < num_m; i++) {
            start = monkeys[i, "s"]
            end = monkeys[i, "n"]
            monkeys[i, "ins"] += (end - start)
            for (j = start; j < end; j++) {
                w = monkeys[i, j]
                delete monkeys[i, j]
                arg = (monkeys[i, "arg"] == "old") ? w : monkeys[i, "arg"]
                if (monkeys[i, "op"] == "*") w *= arg
                else w += arg
                w = int(w / 3)
                target = (w % monkeys[i, "div"] == 0) ? monkeys[i, "t"] : monkeys[i, "f"]
                monkeys[target, monkeys[target, "n"]++] = w
            }
            monkeys[i, "s"] = end
        }
    }
    for (i = 0; i < num_m; i++) {
        val = monkeys[i, "ins"]
        if (val > max1) {
            max2 = max1
            max1 = val
        } else if (val > max2) {
            max2 = val
        }
    }
    printf "%.0f\n", max1 * max2
}

