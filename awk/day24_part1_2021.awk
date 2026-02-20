
BEGIN {
    while ((getline < "input.txt") > 0) {
        if ($1=="div" && $2=="z") l[int(i/18)] = $3
        if ($1=="add" && $2=="x") k[int(i/18)] = $3
        if ($1=="add" && $2=="y") m[int(i/18)] = $3
        i++
    }
    for (i=0; i<14; i++) {
        if (l[i]==1) stack[s++] = i
        else if (l[i]==26) {
            pop = stack[--s]
            c[pop,0] = i
            c[pop,1] = m[pop] + k[i]
        }
    }
    for (i=0; i<14; i++) {
        if (c[i,0]=="") continue
        v = 9
        while (v + c[i,1] > 9) v--
        max[i] = v
        max[c[i,0]] = v + c[i,1]
    }
    n = 0
    for (i=0; i<14; i++) n = n*10 + max[i]
    print n
}
