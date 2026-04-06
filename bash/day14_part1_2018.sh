
#!/bin/bash

awk -v n="$(cat input.txt)" '
BEGIN {
    s[0] = 3; s[1] = 7; l = 2; a = 0; b = 1
    while (l < n + 10) {
        v = s[a] + s[b]
        if (v > 9) {
            s[l++] = 1
            v %= 10
        }
        s[l++] = v
        a = (a + 1 + s[a]) % l
        b = (b + 1 + s[b]) % l
    }
    for (i = n; i < n + 10; i++) printf s[i]
    print ""
}'
