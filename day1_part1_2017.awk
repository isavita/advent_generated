
BEGIN {
    FS=""
}

{
    for(i=1; i<=NF; i++) {
        if($i == $(i%NF+1)) {
            sum += $i
        }
    }
}

END {
    print sum
}
