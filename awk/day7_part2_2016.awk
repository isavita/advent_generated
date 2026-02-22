
BEGIN{
    count=0
    while ((getline line < "input.txt") > 0) {
        hyper=""
        super=line
        while (match(super, /\[[a-z]+\]/)) {
            hyper = hyper substr(super, RSTART+1, RLENGTH-2) " "
            super = substr(super, 1, RSTART-1) substr(super, RSTART+RLENGTH)
        }
        for (i=1; i<=length(super)-2; i++) {
            a=substr(super,i,1); b=substr(super,i+1,1); c=substr(super,i+2,1)
            if (a==c && a!=b) {
                bab=b a b
                if (hyper ~ bab) { count++; break }
            }
        }
    }
    print count
    close("input.txt")
}
