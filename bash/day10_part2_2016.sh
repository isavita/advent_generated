
#!/bin/bash
awk '
$1 == "value" {
    b = "bot " $6
    c[b] = c[b] $2 " "
}
$1 == "bot" {
    b = "bot " $2
    lowTo[b] = $6 " " $7
    highTo[b] = $11 " " $12
}
END {
    active = 1
    while (active) {
        active = 0
        for (b in c) {
            n = split(c[b], chips, " ")
            if (n == 2) {
                active = 1
                v1 = chips[1] + 0; v2 = chips[2] + 0
                lo = (v1 < v2) ? v1 : v2
                hi = (v1 > v2) ? v1 : v2
                delete c[b]
                
                lt = lowTo[b]
                if (lt ~ /^bot/) c[lt] = c[lt] lo " "
                else out[lt] = lo
                
                ht = highTo[b]
                if (ht ~ /^bot/) c[ht] = c[ht] hi " "
                else out[ht] = hi
            }
        }
    }
    print out["output 0"] * out["output 1"] * out["output 2"]
}' input.txt
