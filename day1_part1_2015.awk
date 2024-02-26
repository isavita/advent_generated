
BEGIN {
    FS=""
    floor=0
}
{
    for(i=1; i<=NF; i++) {
        if($i == "(") {
            floor++
        } else if($i == ")") {
            floor--
        }
    }
}
END {
    print "Santa ends up on floor " floor
}
