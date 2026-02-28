
BEGIN{
    fully=0; overlap=0
    while ((getline line < "input.txt") > 0) {
        split(line, p, ",")
        split(p[1], r1, "-")
        split(p[2], r2, "-")
        a=r1[1]; b=r1[2]; c=r2[1]; d=r2[2]
        if (a<=c && b>=d) fully++
        if ((a<=c && c<=b) || (c<=a && a<=d)) overlap++
    }
    print fully
    print overlap
}
