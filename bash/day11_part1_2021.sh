
#!/bin/bash
awk '
{
    n=split($0, a, "")
    for(i=1; i<=n; i++) g[NR,i]=a[i]
}
END {
    for(s=1; s<=100; s++) {
        q_top=0
        for(i=1; i<=10; i++) {
            for(j=1; j<=10; j++) {
                if(++g[i,j] == 10) q[++q_top] = i SUBSEP j
            }
        }
        while(q_top > 0) {
            p = q[q_top--]
            flashes++
            split(p, c, SUBSEP)
            for(y=c[1]-1; y<=c[1]+1; y++) {
                for(x=c[2]-1; x<=c[2]+1; x++) {
                    if((y,x) in g && ++g[y,x] == 10) q[++q_top] = y SUBSEP x
                }
            }
        }
        for(i=1; i<=10; i++) {
            for(j=1; j<=10; j++) {
                if(g[i,j] > 9) g[i,j] = 0
            }
        }
    }
    print flashes
}' input.txt
