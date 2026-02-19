
#!/usr/bin/awk -f
BEGIN {
    while ((getline line < "input.txt") > 0) {
        if (line == "") continue
        gsub(/[(),]/, " ", line)
        split(line, a, " ")
        name = a[1]
        weight[name] = a[2] + 0
        all[name] = 1
        if (a[3] == "->") {
            for (i = 4; i <= length(a); i++) {
                if (a[i] == "") continue
                holds[name] = holds[name] " " a[i]
                children[a[i]] = 1
            }
        }
    }
    for (r in all) if (!(r in children)) root = r
    dfs(root)
}
function dfs(n,    total, i, child, cw, w1, w2, cnt1, cnt2, arr, wcnt, target, correct) {
    total = weight[n]
    split(holds[n], arr, " ")
    delete wcnt
    for (i in arr) {
        child = arr[i]
        if (child == "") continue
        cw = dfs(child)
        total += cw
        wcnt[cw]++
    }
    totalWeight[n] = total
    if (length(wcnt) > 1 && !done) {
        for (w1 in wcnt) for (w2 in wcnt)
            if (w1 != w2 && wcnt[w1] < wcnt[w2]) {
                target = w1 + 0; correct = w2 + 0
                for (i in arr) {
                    child = arr[i]
                    if (child == "") continue
                    if (totalWeight[child] == target) {
                        print weight[child] + (correct - target)
                        done = 1
                        exit
                    }
                }
            }
    }
    return total
}
