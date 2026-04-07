
#!/bin/bash
awk '{
    for(i=1; i<=NF; i++) b[i-1]=$i; n=NF
    while(1) {
        s=""; for(i=0; i<n; i++) s=s b[i]"_"
        if(s in seen) break
        seen[s]=c++
        m=0; for(i=1; i<n; i++) if(b[i]>b[m]) m=i
        v=b[m]; b[m]=0
        for(i=1; i<=v; i++) b[(m+i)%n]++
    }
    printf "Part 1: %d\nPart 2: %d\n", c, c-seen[s]
}' input.txt
