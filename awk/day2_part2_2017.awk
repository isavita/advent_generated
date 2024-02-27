
# Part 1
FILENAME=="input.txt" {
    min=100000000000000
    max=0
    for(i=1; i<=NF; i++) {
        if($i < min) {
            min=$i
        }
        if($i > max) {
            max=$i
        }
    }
    diff+=max-min
    min=100000000000000
    max=0
}
END {
    print "Part 1: Checksum is " diff
}

# Part 2
FILENAME=="input.txt" {
    for(i=1; i<=NF; i++) {
        for(j=1; j<=NF; j++) {
            if(i != j && $i % $j == 0) {
                sum+=$i/$j
                next
            }
        }
    }
}
END {
    print "Part 2: Sum is " sum
}
