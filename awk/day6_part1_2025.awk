
#!/usr/bin/awk -f
BEGIN{
    while ((getline line < "input.txt") > 0) {
        ++n; L[n]=line; len[n]=length(line);
        if (len[n] > max) max=len[n];
    }
    close("input.txt");

    for (c=1; c<=max; c++) {
        sep[c]=1;
        for (i=1; i<=n && sep[c]; i++) {
            if (c<=len[i] && substr(L[i],c,1) !~ /[ \t]/) sep[c]=0;
        }
    }

    total=0; inb=0;
    for (c=1; c<=max; c++) {
        if (!sep[c]) {
            if (!inb) { inb=1; start=c }
        } else if (inb) {
            total+=block(start,c-1);
            inb=0
        }
    }
    if (inb) total+=block(start,max);
    print "Grand total: " total
}
function trim(s){ sub(/^([ \t]*)/, "", s); sub(/([ \t]*)$/, "", s); return s }
function block(s,e,   i, seg, op, cnt, sum, prod, val){
    op=""; cnt=0; sum=0; prod=1;
    for (i=1; i<=n; i++) {
        if (s<=len[i]) {
            seg=trim(substr(L[i],s, e-s+1));
            if (seg!="") {
                if (seg=="+" || seg=="*") op=seg;
                else if (seg ~ /^[0-9]+$/) {
                    ++cnt;
                    val=seg+0;
                    sum+=val;
                    prod*=val;
                }
            }
        }
    }
    if (cnt==0) return 0;
    if (op=="+") return sum;
    if (op=="*") return prod;
    if (cnt==1) return sum;
    return 0
}
