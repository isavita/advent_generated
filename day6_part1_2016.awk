
BEGIN {
    FS=""
}
{
    for(i=1; i<=NF; i++) {
        count[$i,i]++
    }
}
END {
    for(i=1; i<=NF; i++) {
        max=0
        for(j in count) {
            split(j, arr, SUBSEP)
            if(arr[2]==i && count[j]>max) {
                max=count[j]
                max_char=arr[1]
            }
        }
        printf("%s", max_char)
    }
    print ""
}
