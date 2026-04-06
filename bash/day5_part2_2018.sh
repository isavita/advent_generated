
#!/bin/bash
awk '
function react(s,  a,st,n,l,i,c,t,d) {
    n = split(s, a, "")
    l = 0
    for (i = 1; i <= n; i++) {
        c = a[i]
        if (l > 0) {
            t = st[l]
            d = ord[c] - ord[t]
            if (d == 32 || d == -32) {
                l--
                continue
            }
        }
        st[++l] = c
    }
    return l
}
BEGIN {
    for (i = 0; i < 256; i++) ord[sprintf("%c", i)] = i
}
{
    gsub(/[^a-zA-Z]/, "")
    poly = $0
    p1 = react(poly)
    print p1
    
    min_len = p1
    for (i = 97; i <= 122; i++) {
        char_l = sprintf("%c", i)
        char_u = sprintf("%c", i - 32)
        temp = poly
        gsub("[" char_l char_u "]", "", temp)
        res = react(temp)
        if (res < min_len) min_len = res
    }
    print min_len
}' input.txt
