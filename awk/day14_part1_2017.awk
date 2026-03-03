function x(a,b,r,p,i){r=0;p=1;for(i=0;i<8;i++){if(a%2!=b%2)r+=p;a=int(a/2);b=int(b/2);p*=2}return r}
BEGIN {
    if ((getline k < "input.txt") <= 0) exit
    sub(/[ \t\r\n]+$/, "", k)
    for(i=0; i<128; i++) o[sprintf("%c",i)]=i
    for(i=0; i<256; i++) {
        c=0; t=i; while(t>0){if(t%2)c++; t=int(t/2)}
        bc[i]=c
    }
    for(row=0; row<128; row++) {
        s=k"-"row; m=length(s)
        for(i=1; i<=m; i++) ln[i-1]=o[substr(s,i,1)]
        ln[m]=17; ln[m+1]=31; ln[m+2]=73; ln[m+3]=47; ln[m+4]=23
        for(i=0; i<256; i++) l[i]=i
        p=0; sk=0
        for(rnd=0; rnd<64; rnd++) {
            for(i=0; i<m+5; i++) {
                L=ln[i]
                for(j=0; j<int(L/2); j++) {
                    a1=(p+j)%256; a2=(p+L-1-j)%256
                    t=l[a1]; l[a1]=l[a2]; l[a2]=t
                }
                p=(p+L+sk)%256; sk++
            }
        }
        for(i=0; i<16; i++) {
            v=0; for(j=0; j<16; j++) v=x(v, l[i*16+j])
            ans+=bc[v]
        }
    }
    print ans
}