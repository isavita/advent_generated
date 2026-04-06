
awk 'BEGIN { FS="" }
{ for(i=1;i<=NF;i++) g[++n]=$i }
END {
    while(1) {
        s=0; p=1
        for(i=1;i<=25;i++) { if(g[i]=="#") s+=p; p*=2 }
        if(seen[s]++) { printf "%.0f\n", s; exit }
        for(i=1;i<=25;i++) {
            c=(i>5 && g[i-5]=="#") + (i<21 && g[i+5]=="#") + \
              ((i-1)%5!=0 && g[i-1]=="#") + (i%5!=0 && g[i+1]=="#")
            nx[i]=(g[i]=="#" ? (c==1?"#":".") : (c==1||c==2?"#":"."))
        }
        for(i=1;i<=25;i++) g[i]=nx[i]
    }
}' input.txt
