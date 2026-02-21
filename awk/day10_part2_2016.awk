#!/usr/bin/awk -f
BEGIN{
    while ((getline line < "input.txt") > 0) {
        split(line, f, " ")
        if (f[1] == "value") {
            val = f[2]; bot = "bot " f[6]
            chips[bot] = chips[bot] " " val
        } else if (f[1] == "bot") {
            bot = "bot " f[2]
            lowTo[bot] = f[6] " " f[7]
            highTo[bot] = f[11] " " f[12]
        }
    }
    changed = 1
    while (changed) {
        changed = 0
        for (b in chips) {
            split(chips[b], a, " ")
            cnt = 0
            for (i in a) if (a[i] != "") cnt++
            if (cnt == 2) {
                low = a[1] + 0; high = a[2] + 0
                if (low > high) { t = low; low = high; high = t }
                chips[b] = ""
                give(low, lowTo[b])
                give(high, highTo[b])
                changed = 1
            }
        }
    }
    result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"]
    print result
}
function give(v, tgt) {
    if (tgt ~ /^bot/) {
        chips[tgt] = chips[tgt] " " v
    } else if (tgt ~ /^output/) {
        outputs[tgt] = v
    }
}