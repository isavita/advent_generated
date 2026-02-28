
#!/usr/bin/awk -f
BEGIN{
    parsing=1; nr=0; nid=0
    while ((getline line < "input.txt") > 0) {
        gsub(/[ \t\r]+/, "", line)
        if (line == "") {
            if (nr>0) parsing=0
            continue
        }
        if (parsing && index(line, "-")) {
            split(line, a, "-")
            nr++; start[nr]=a[1]+0; end[nr]=a[2]+0
        } else {
            parsing=0; nid++; ids[nid]=line+0
        }
    }
    close("input.txt")
    for (i=1;i<=nr;i++) order[i]=i
    for (i=2;i<=nr;i++) {
        key=order[i]; j=i-1
        while (j>=1 && start[order[j]] > start[key]) {
            order[j+1]=order[j]; j--
        }
        order[j+1]=key
    }
    m=0
    for (i=1;i<=nr;i++) {
        idx=order[i]; s=start[idx]; e=end[idx]
        if (m==0) { m++; ms[m]=s; me[m]=e }
        else if (s <= me[m]) { if (e > me[m]) me[m]=e }
        else { m++; ms[m]=s; me[m]=e }
    }
    fresh=0
    for (i=1;i<=nid;i++) {
        id=ids[i]
        for (j=1;j<=m;j++) {
            if (id < ms[j]) break
            if (id >= ms[j] && id <= me[j]) { fresh++; break }
        }
    }
    total=0
    for (j=1;j<=m;j++) total+= (me[j]-ms[j]+1)
    print "Part One: " fresh
    print "Part Two: " total
}
