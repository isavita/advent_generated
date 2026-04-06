
#!/bin/bash
awk '
BEGIN {
    v["("] = 1; v["["] = 2; v["{"] = 3; v["<"] = 4
}
{
    t = 0; split("", s); err = 0
    for (i = 1; i <= length($0); i++) {
        c = substr($0, i, 1)
        if (c in v) {
            s[++t] = c
        } else {
            if (t == 0) { err = 1; break }
            o = s[t--]
            if ((c == ")" && o != "(") || (c == "]" && o != "[") || 
                (c == "}" && o != "{") || (c == ">" && o != "<")) {
                err = 1; break
            }
        }
    }
    if (!err && t > 0) {
        sc = 0
        while (t > 0) sc = sc * 5 + v[s[t--]]
        print sc
    }
}' input.txt | sort -n | awk '{a[NR]=$1} END {if(NR>0) print a[int((NR+1)/2)]}'
