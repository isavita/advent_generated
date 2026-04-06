
awk '
BEGIN { SUBSEP="," }
{
    split($0, chars, "")
    for (x=1; x<=length($0); x++) if (chars[x]=="#") active[x,NR,0,0]=1
}
END {
    for (i=0; i<6; i++) {
        delete count
        for (coord in active) {
            split(coord, s, ",")
            for (w=-1; w<=1; w++)
                for (z=-1; z<=1; z++)
                    for (y=-1; y<=1; y++)
                        for (x=-1; x<=1; x++)
                            if (x||y||z||w) count[s[1]+x, s[2]+y, s[3]+z, s[4]+w]++
        }
        delete next_active
        for (coord in count)
            if (count[coord]==3 || (count[coord]==2 && coord in active))
                next_active[coord]=1
        delete active
        for (coord in next_active) active[coord]=1
    }
    for (coord in active) sum++
    print sum
}' input.txt
