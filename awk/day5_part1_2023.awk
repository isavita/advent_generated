
BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($0 ~ /map:/) {
            ++mapCnt
            rangeCnt[mapCnt]=0
        }
        else if ($0 ~ /^seeds:/) {
            for (i=2;i<=NF;i++) seeds[++seedCnt]=$i
        }
        else if (NF==3) {
            dest=1+$0
            split($0, a, " ")
            ++rangeCnt[mapCnt]
            rsrc[mapCnt,rangeCnt[mapCnt]]   =a[2]
            rdest[mapCnt,rangeCnt[mapCnt]]  =a[1]
            rlen[mapCnt,rangeCnt[mapCnt]]   =a[3]
        }
    }
    minloc=-1
    for (s=1;s<=seedCnt;s++) {
        loc=seeds[s]
        for (m=1;m<=mapCnt;m++) {
            for (i=1;i<=rangeCnt[m];i++) {
                src=rsrc[m,i]; dst=rdest[m,i]; len=rlen[m,i]
                if (loc>=src && loc<src+len) {
                    loc=dst+(loc-src)
                    break
                }
            }
        }
        if (minloc==-1 || loc<minloc) minloc=loc
    }
    print minloc
}
